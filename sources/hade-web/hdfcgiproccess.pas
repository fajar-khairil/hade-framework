unit hdfcgiproccess;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  blcksock,
  synsock,
  fastcgi,
  custweb,
  contnrs,
  custfcgi,
  //syncobjs,
  httpdefs;

type
  EHadeFCGIException = class(Exception);
  THadeFCGIProccessManager = class;
  TUnknownRecordEvent = procedure(ARequest: TFCGIRequest;
    AFCGIRecord: PFCGI_Header) of object;

  { THadeFCGIRequest }

  THadeFCGIRequest = class(TFCGIRequest)
  protected
    FLog: TLogEvent;

  public
    FKeepConnectionAfterRequest: boolean;
    function ProcessFCGIRecord(AFCGIRecord: PFCGI_Header): boolean; override;

    property OnLog: TLogEvent read FLog write FLog;
  end;

  { THadeFCGIResponse }

  THadeFCGIResponse = class(TFCGIResponse)
  protected
    FOnWrite: TFastCGIWriteEvent;
    procedure Write_FCGIRecord(ARecord: PFCGI_Header); override;
  public
    property OnWrite: TFastCGIWriteEvent read FOnWrite write FOnWrite;
  end;

  //maintaing request id
  THadeRequestResponse = record
    Request: THadeFCgiRequest;
    Response: THadeFCgiResponse;
  end;

  { THadeFCGIHandler }

  THadeFCGIHandler = class(TWebHandler)
  protected
    FAddress: string;
    FLingerTimeOut: integer;
    FOnUnknownRecord: TUnknownRecordEvent;
    FOnTerminate: TNotifyEvent;
    FPort: integer;
    FTimeOut: integer;
    FProcManager: THadeFCGIProccessManager;
    FRequestsArray: array of THadeRequestResponse;
    FRequestsAvail: integer;

    FSock: TTCPBlockSocket;
    FOnError: TNotifyEvent;
    //events triger
    procedure DoHandleError;
    procedure DoOnIdle;

    //Error Handler
    procedure RaiseError(AMsg: string; closeSocket: boolean = True);

    procedure CloseSocket;
    procedure SetupSocket;
    //hide hint message abstract error
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean;override;

    function getMaxcount: integer;
    procedure setMaxCount(AValue: integer);
  public
    //events properties
    property OnError: TNotifyEvent read FOnError write FOnError;

    property Port: integer read FPort write FPort;
    property LingerTimeOut: integer read FLingerTimeOut write FLingerTimeOut;
    property Address: string read FAddress write FAddress;
    property OnUnknownRecord: TUnknownRecordEvent
      read FOnUnknownRecord write FOnUnknownRecord;
    property TimeOut: integer read FTimeOut write FTimeOut;
    property MaxThreadCount: integer read getMaxcount write setMaxCount;

    //main loop
    procedure Run; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { THadeFCGIProccess }

  THadeFCGIProccess = class(TThread)
  protected
    FSock: TTCPBlockSocket;
    FDone: boolean;
    FManager: THadeFCGIProccessManager;
    FHandler: THadeFCGIHandler;
    FWebHandler: TWebHandler;

    procedure MarkDone;
    procedure MarkUndone;

    //fastcgi stuff
    function WaitForRequest(out ARequest: THadeFCGIRequest;
      out AResponse: THadeFCGIResponse): boolean;
    function Read_FCGIRecord: PFCGI_Header;
    function DoFastCGIRead(AHandle: TSocket; var ABuf; ACount: integer): integer;
    function DoFastCGIWrite(AHandle: TSocket; const ABuf; ACount: integer;
      Out ExtendedErrorCode: integer): integer;
    function ProcessRecord(AFCGI_Record: PFCGI_Header;
      out ARequest: THadeFCGIRequest; out AResponse: THadeFCGIResponse): boolean;

    procedure EndRequest(ARequest : TRequest;AResponse : TResponse);
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse);
    Procedure DoHandleRequest(ARequest : TRequest; AResponse : TResponse);
  public
    property Done: boolean read FDone;

    procedure Execute; override;

    procedure SetHandler(AHandler: THadeFCGIHandler);
    procedure SetSocket(ASocket: TSocket);

    constructor Create(AManager: THadeFCGIProccessManager);
    destructor Destroy; override;
  end;

  { THadeFCGIProccessManager }

  THadeFCGIProccessManager = class
  protected
    FHandler: THadeFCGIHandler;
    FList: TFPObjectList;
    FMaxCount:Integer;
    //FCritical:TCriticalSection;

    procedure CleanAll;
    procedure setMaxCount(AValue: integer);
  public
    //property Critical : TCriticalSection read FCritical;
    property MaxCount:integer read FMaxCount write setMaxCount;
    procedure CleanProccess(ASender: TObject);
    function Count: integer;
    function ActiveCount: integer;
    function Proccess(ASock: TSocket): THadeFCGIProccess;

    constructor Create(AHandler: THadeFCGIHandler;AMaxCount:integer = 20);
    destructor Destroy; override;
  end;

implementation

uses
  baseunix,
  fphttp;

{ THadeFCGIRequest }

function THadeFCGIRequest.ProcessFCGIRecord(AFCGIRecord: PFCGI_Header): boolean;
begin
  Result := inherited ProcessFCGIRecord(AFCGIRecord);
end;

{ THadeFCGIResponse }

procedure THadeFCGIResponse.Write_FCGIRecord(ARecord: PFCGI_Header);
var
  ErrorCode, BytesToWrite, BytesWritten: integer;
  P: PByte;
  r: THadeFCGIRequest;

begin
  if not (Request is THadeFCGIRequest) then
    raise EHadeFCGIException.Create('request object invalid');
  R := THadeFCGIRequest(Request);
  BytesToWrite := BEtoN(ARecord^.contentLength) + ARecord^.paddingLength +
    sizeof(FCGI_Header);
  P := PByte(Arecord);
  repeat
    BytesWritten := FOnWrite(R.Handle, P^, BytesToWrite, ErrorCode);
    if (BytesWritten < 0) then
    begin
      // TODO : Better checking on ErrorCode
      R.FKeepConnectionAfterRequest := False;
      raise EHadeFCGIException.Create('something went wrong BytesWritten < 0');
    end;
    Inc(P, BytesWritten);
    Dec(BytesToWrite, BytesWritten);
  until (BytesToWrite = 0) or (BytesWritten = 0);
end;

{ THadeFCGIRequest }

{ THadeFCGIProccess }

procedure THadeFCGIProccess.MarkDone;
begin
  FDone := True;
  FSock.CloseSocket;
  Self.Terminate;
end;

function THadeFCGIProccess.WaitForRequest(out ARequest: THadeFCGIRequest;
  out AResponse: THadeFCGIResponse): boolean;
var
  AFCGI_Record: PFCGI_Header;
begin
  Result := False;
  AResponse := nil;
  ARequest := nil;
  repeat
    AFCGI_Record := Read_FCGIRecord;
    if not Assigned(AFCGI_Record) then
    begin
      raise EHadeFCGIException.Create('Close Gracefully..');
    end
    else
    begin
      try
        Result := ProcessRecord(AFCGI_Record, ARequest, AResponse);
      finally
        FreeMem(AFCGI_Record);
        AFCGI_Record := nil;
      end;
    end;
  until Result or (FSock.Socket = TSocket(-1));
end;

function THadeFCGIProccess.Read_FCGIRecord: PFCGI_Header;

  function ReadBytes(ReadBuf: Pointer; ByteAmount: word): integer;

  var
    P: PByte;
    Count: integer;

  begin
    Result := 0;
    P := ReadBuf;
    if (ByteAmount = 0) then
      exit;
    repeat
      Count := DoFastCGIRead(FSock.Socket, P^, ByteAmount);
      if (Count > 0) then
      begin
        Dec(ByteAmount, Count);
        P := P + Count;
        Inc(Result, Count);
      end
      else if (Count < 0) then
        raise EHadeFCGIException.Create('Failed to read Header');
    until (ByteAmount = 0) or (Count = 0);
  end;

var
  Header: FCGI_Header;
  BytesRead: integer;
  ContentLength: word;
  PaddingLength: byte;
  ResRecord: pointer;
  ReadBuf: pointer;
begin
  Result := nil;
  ResRecord := nil;
  ReadBuf := @Header;
  BytesRead := ReadBytes(ReadBuf, Sizeof(Header));
  if (BytesRead = 0) then
    Exit // Connection closed gracefully.
  // TODO : if connection closed gracefully, the request should no longer be handled.
  // Need to discard request/response
  else if (BytesRead <> Sizeof(Header)) then
    raise EHadeFCGIException.Create('Failed reading header');

  ContentLength := BetoN(Header.contentLength);
  PaddingLength := Header.paddingLength;
  Getmem(ResRecord, BytesRead + ContentLength + PaddingLength);
  try
    PFCGI_Header(ResRecord)^ := Header;
    ReadBuf := ResRecord + BytesRead;
    BytesRead := ReadBytes(ReadBuf, ContentLength);
    if (BytesRead = 0) and (ContentLength > 0) then
    begin
      FreeMem(resRecord);
      Exit; // Connection closed gracefully.
      // TODO : properly handle connection close
    end;
    ReadBuf := ReadBuf + BytesRead;
    BytesRead := ReadBytes(ReadBuf, PaddingLength);
    if (BytesRead = 0) and (PaddingLength > 0) then
    begin
      FreeMem(resRecord);
      Exit; // Connection closed gracefully.
      // TODO : properly handle connection close
    end;
    Result := ResRecord;
  except
    FreeMem(resRecord);
    raise;
  end;
end;

function THadeFCGIProccess.DoFastCGIRead(AHandle: TSocket; var ABuf;
  ACount: integer): integer;
begin
  {$ifdef windowspipe}
  if FIsWinPipe then
    Result := FileRead(AHandle, ABuf, ACount)
  else
  {$endif}
    //Result:=sockets.fpRecv(AHandle, @Abuf, ACount, NoSignalAttr);
    Result := FSock.RecvBuffer(@Abuf, ACount);
end;

function THadeFCGIProccess.DoFastCGIWrite(AHandle: TSocket; const ABuf;
  ACount: integer; out ExtendedErrorCode: integer): integer;
begin
  {$ifdef windowspipe}
  if FIsWinPipe then
  begin
    ExtendedErrorCode := 0;
    Result := FileWrite(AHandle, ABuf, ACount);
    if (Result < 0) then
      ExtendedErrorCode := GetLastOSError;
  end
  else
  {$endif windows}
  begin
    repeat
      ExtendedErrorCode := 0;
      //Result:=sockets.fpsend(AHandle, @ABuf, ACount, NoSignalAttr);
      Result := FSock.SendBuffer(@ABuf, ACount);
      if (Result < 0) then
        ExtendedErrorCode := FSock.LastError;
      //ExtendedErrorCode:=sockets.socketerror;
    until (Result >= 0)
{$ifdef unix}
      or (ExtendedErrorCode <> ESysEINTR);
{$endif}
  end;
end;

function THadeFCGIProccess.ProcessRecord(AFCGI_Record: PFCGI_Header;
  out ARequest: THadeFCGIRequest; out AResponse: THadeFCGIResponse): boolean;
var
  ARequestID: word;
  ATempRequest: THadeFCGIRequest;
begin
  Result := False;
  ARequestID := BEtoN(AFCGI_Record^.requestID);
  if AFCGI_Record^.reqtype = FCGI_BEGIN_REQUEST then
  begin
    if ARequestID > FHandler.FRequestsAvail then
    begin
      Inc(FHandler.FRequestsAvail, 10);
      SetLength(FHandler.FRequestsArray, FHandler.FRequestsAvail);
    end;
    assert(not assigned(FHandler.FRequestsArray[ARequestID].Request));
    assert(not assigned(FHandler.FRequestsArray[ARequestID].Response));
    ATempRequest := THadeFCGIRequest.Create;
    ATempRequest.RequestID := ARequestID;
    ATempRequest.Handle := FSock.Socket;
    //ATempRequest.ProtocolOptions:=FHandler.Protocoloptions;
    ATempRequest.OnUnknownRecord := FHandler.OnUnknownRecord;
    ATempRequest.Onlog := @FHandler.Log;
    FHandler.FRequestsArray[ARequestID].Request := ATempRequest;
  end;

  if (ARequestID > FHandler.FRequestsAvail) then
  begin
    // TODO : ARequestID can be invalid. What to do ?
    // in each case not try to access the array with requests.
  end
  else if FHandler.FRequestsArray[ARequestID].Request.ProcessFCGIRecord(
    AFCGI_Record) then
  begin
    ARequest := FHandler.FRequestsArray[ARequestID].Request;
    FHandler.FRequestsArray[ARequestID].Response :=
      THadeFCGIResponse.Create(THadeFCGIRequest(ARequest));
    //FHandler.FRequestsArray[ARequestID].Response.ProtocolOptions:=Self.ProtocolOptions;
    FHandler.FRequestsArray[ARequestID].Response.FOnWrite := @DoFastCGIWrite;
    AResponse := FHandler.FRequestsArray[ARequestID].Response;
    Result := True;
  end;
end;

procedure THadeFCGIProccess.EndRequest(ARequest: TRequest; AResponse: TResponse
  );
begin
  with FHandler.FRequestsArray[THadeFCGIRequest(ARequest).RequestID] do
  begin
    Assert(ARequest=Request);
    Assert(AResponse=Response);
    if (not THadeFCGIRequest(ARequest).KeepConnectionAfterRequest) then
      Self.MarkDone;
    ARequest := Nil;
    AResponse := Nil;
  end;
  ARequest.Free;
  AResponse.Free;
end;

procedure THadeFCGIProccess.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
Var
  MC : TCustomHTTPModuleClass;
  M  : TCustomHTTPModule;
  MN : String;
  MI : TModuleItem;
begin
  try
    MC:=Nil;
    M:=NIL;
    MI:=Nil;
    If (FHandler.OnGetModule<>Nil) then
      FHandler.OnGetModule(FHandler,ARequest,MC);
    If (MC=Nil) then
      begin
      MN:=FHandler.GetModuleName(ARequest);
      MI:=ModuleFactory.FindModule(MN);
      if (MI=Nil) then
        Raise EFPWebError.CreateFmt('Could not determine HTTP module for request "%s"',[MN]);
      MC:=MI.ModuleClass;
      end;
    M:=FHandler.FindModule(MC); // Check if a module exists already
    If (M=Nil) then
      if assigned(MI) and Mi.SkipStreaming then
        M:=MC.CreateNew(FHandler)
      else
        M:=MC.Create(FHandler);
    FHandler.SetBaseURL(M,MN,ARequest);
    if (FHandler.OnInitModule<>Nil) then
      FHandler.OnInitModule(FHandler,M);
    M.DoAfterInitModule(ARequest);
    if M.Kind=wkOneShot then
      begin
      try
        M.HandleRequest(ARequest,AResponse);
      finally
        M.Free;
      end;
      end
    else
      M.HandleRequest(ARequest,AResponse);
  except
    On E : Exception do
      FHandler.ShowRequestException(AResponse,E);
  end;
end;

procedure THadeFCGIProccess.DoHandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  Try
    Self.HandleRequest(ARequest,AResponse);
    If Not AResponse.ContentSent then
      try
        AResponse.SendContent;
      except
        On E : Exception do
          FHandler.Log(etError,Format('An error ocurred when sending request : %s %s',[E.ClassName,E.Message]));
      end;
  Finally
    Self.EndRequest(ARequest,AResponse);
  end;
end;


procedure THadeFCGIProccess.MarkUndone;
begin
  FDone := False;
end;

procedure THadeFCGIProccess.Execute;
var
  AResponse: THadeFCGIResponse;
  ARequest: THadeFCGIRequest;
  WaitOK: boolean;
begin
  WaitOK := False;
  while (not Terminated) and (not Self.Done) do
  begin
    WaitOK := Self.WaitForRequest(ARequest, AResponse);
    if WaitOK then
    begin
        Self.DoHandleRequest(ARequest, AResponse);
    end;
    sleep(1);
  end;
end;

procedure THadeFCGIProccess.SetHandler(AHandler: THadeFCGIHandler);
begin
  FHandler := AHandler;
end;

procedure THadeFCGIProccess.SetSocket(ASocket: TSocket);
begin
  FSock.Socket := ASocket;
end;

constructor THadeFCGIProccess.Create(AManager: THadeFCGIProccessManager);
begin
  FSock := TTCPBlockSocket.Create;
  FSock.Family := SF_IP4;
  FManager := AManager;
  self.Priority := tpNormal;
  Self.MarkUndone;
  self.FreeOnTerminate := False;
  inherited Create(True);
end;

destructor THadeFCGIProccess.Destroy;
begin
  FSock.CloseSocket;
  FSock.Free;
  inherited Destroy;
end;

{ THadeFCGIProccessManager }

procedure THadeFCGIProccessManager.setMaxCount(AValue: integer);
begin
  if (FMaxCount < 1) and (FMaxCount = AValue) then Exit;
  FMaxCount:= AValue;
end;

procedure THadeFCGIProccessManager.CleanAll;
var
  iloop: integer;
  cp: THadeFCGIProccess;
begin
  for iloop := 0 to pred(Count) do
  begin
    cp := THadeFCGIProccess(FList.Items[iloop]);
    if not cp.Done then
    begin
      cp.WaitFor;
      cp.Terminate;
      cp.Free;
    end
    else
    begin
      cp.Terminate;
      cp.Free;
    end;
  end;

  FList.Clear;
end;

procedure THadeFCGIProccessManager.CleanProccess(ASender: TObject);
var
  iloop, icount: integer;
  cp: THadeFCGIProccess;
  idx: array of integer;
  jloop: integer;
begin
  icount := 0;
  setLength(idx, Count);
  for iloop := 0 to pred(Count) do
  begin
    cp := THadeFCGIProccess(FList[iloop]);
    if cp.Done then
    begin
      //WriteLn('Release thread at index : ' + IntToStr(iloop));
      cp.Terminate;
      cp.Free;
      idx[icount] := iloop;
    end;
    Inc(icount);
  end;

  for jloop := LOW(idx) to HIGH(idx) do
    FList.Delete(idx[jloop]);
end;

function THadeFCGIProccessManager.Count: integer;
begin
  Result := Flist.Count;
end;

function THadeFCGIProccessManager.ActiveCount: integer;
var
  iloop: integer;
begin
  Result := 0;
  for iloop := 0 to pred(Count) do
    if not THadeFCGIProccess(FList[iloop]).Done then
      Inc(Result);
end;

function THadeFCGIProccessManager.Proccess(ASock: TSocket): THadeFCGIProccess;
begin
  Result := nil;

  //WriteLn('Count : '+IntToStr(count));
  //WriteLn('Max Count : '+IntToStr(FMaxCount));
  //if max thread count is reached then just exit it will wait thread to finish
  if Count > FMaxCount then begin WriteLn('Max Thread Count is reached..'); exit; end;

  Result := THadeFCGIProccess.Create(Self);
  Self.FList.Add(Result);

  //set needed property and start execute
  Result.SetSocket(ASock);
  Result.SetHandler(FHandler);
  Result.start;
end;

constructor THadeFCGIProccessManager.Create(AHandler: THadeFCGIHandler;
  AMaxCount: integer);
begin
  FHandler := AHandler;
  FList := TFPObjectList.Create(False);
  FMaxCount:= AMaxCount;
  //FCritical := TCriticalSection.Create;
end;

destructor THadeFCGIProccessManager.Destroy;
begin
  Self.CleanAll;
  FList.Free;
  //FCritical.Free;
  inherited Destroy;
end;

{ THadeFCGIHandler }

function THadeFCGIHandler.getMaxcount: integer;
begin
  Result:= FProcManager.MaxCount;
end;

procedure THadeFCGIHandler.setMaxCount(AValue: integer);
begin
  FProcManager.MaxCount:= AValue;
end;

procedure THadeFCGIHandler.DoHandleError;
begin
  if Assigned(FOnError) then
    OnError(self);
end;

procedure THadeFCGIHandler.DoOnIdle;
begin
  if Assigned(self.OnIdle) then
    Self.OnIdle(self);
  FProcManager.CleanProccess(self);
end;

procedure THadeFCGIHandler.RaiseError(AMsg: string; closeSocket: boolean);
begin
  //WriteLn(AMsg);
  Self.DoHandleError;
  if closeSocket then
    Self.CloseSocket;
  raise EHadeFCGIException.Create(AMsg);
end;

procedure THadeFCGIHandler.CloseSocket;
begin
  FSock.CloseSocket;
end;

procedure THadeFCGIHandler.SetupSocket;
begin
  FSock.CreateSocket;
  FSock.SetLinger(True, self.FLingerTimeOut);
  FSock.SetTimeout(self.FTimeOut);
  if self.FAddress = EmptyStr then
    FSock.Bind(cAnyHost, IntToStr(FPort))
  else
    FSock.Bind(self.Address, IntToStr(FPort));

  if FSock.LastError <> 0 then
    self.RaiseError('Failed to bind at port ' + IntToStr(FPort) +
      ' : ' + FSock.GetErrorDescEx);

  FSock.Listen;

  if FSock.LastError <> 0 then
    self.RaiseError('Failed to listen : ' + FSock.GetErrorDescEx);
end;

function THadeFCGIHandler.WaitForRequest(out ARequest: TRequest; out
  AResponse: TResponse): boolean;
begin
  //its should never called
end;

procedure THadeFCGIHandler.Run;
var
  cSock: TSocket;
begin
  Self.SetupSocket;
  while not Self.Terminated do
  begin
    if FSock.CanRead(10000) then
    begin

      repeat
        cSock := FSock.Accept;
      until cSock <> -1;

      if FSock.LastError = 0 then
      begin
        FProcManager.Proccess(cSock);
        //WriteLn('Thread Count : ' + IntToStr(FProcManager.Count));
      end
      else
        Self.RaiseError('Failed To Accept : ' + FSock.GetErrorDescEx, False);//False mean don't close the socket
    end;
    sleep(1);
    Self.DoOnIdle;
  end;
end;

constructor THadeFCGIHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSock := TTCPBlockSocket.Create;
  //default properties
  FLingerTimeOut := 1000;
  FTimeOut := 10000;
  FPort := 9090;
  FProcManager := THadeFCGIProccessManager.Create(self);
  FRequestsAvail := FProcManager.MaxCount;
  SetLength(FRequestsArray, FRequestsAvail);
end;

destructor THadeFCGIHandler.Destroy;
begin
  SetLength(FRequestsArray, 0);
  //Self.CloseSocket;
  FSock.Free;
  FProcManager.Free;
  inherited Destroy;
end;

end.
