unit hddatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdbase,
  hdpersistentintf,
  hdconnection,
  hdobject,
  hdquery,
  hdimplementor;
CONST
  fcLazy = TFetchMode.fcLazy;
  fcJoin = TFetchMode.fcJoin;
type
  EHadeDatabaseException = class(EHadeException);

  TUpdateMode = (upSkipError,upStopError);

  { THadeDatabase }

  THadeDatabase = class(THadeBaseObject, IHadePersistent)
  protected
    FOnConnect:TNotifyEvent;
    FOnDisconnect:TNotifyEvent;
    FOnConnectError:TNotifyEvent;
    FOnError:TNotifyEvent;
    FOnCommit:TNotifyEvent;
    FOnBeforeRead: TNotifyEvent;
    FOnBeforeSave: TNotifyEvent;

    FQuery: THadeQuery;
    fConnection: THadeConnection;
    FImplementor:THadeImplementor;
    fConnectionName: string;

    procedure InitConnection;
    procedure ReturnConnection;
    procedure RaiseError(const AMsg: string);
    procedure SetOnBeforeRead(AValue: TNotifyEvent);
    procedure SetOnBeforeSave(AValue: TNotifyEvent);
    procedure SetOnCommit(AValue: TNotifyEvent);
    procedure SetOnConnect(AValue: TNotifyEvent);
    procedure SetOnConnectError(AValue: TNotifyEvent);
    procedure SetOnDisconnect(AValue: TNotifyEvent);
    procedure SetOnError(AValue: TNotifyEvent);
  public
    //event related properties
    property OnConnect:TNotifyEvent read FOnConnect write SetOnConnect;
    property OnDisconnect:TNotifyEvent read FOnDisconnect write SetOnDisconnect;
    property OnCommit:TNotifyEvent read FOnCommit write SetOnCommit;
    property OnBeforeRead:TNotifyEvent read FOnBeforeRead write SetOnBeforeRead;
    property OnBeforeSave:TNotifyEvent read FOnBeforeSave write SetOnBeforeSave;
    property OnConnectError:TNotifyEvent read FOnConnectError write SetOnConnectError;
    property OnError:TNotifyEvent read FOnError write SetOnError;

    property Query:THadeQuery read FQuery;

    procedure StartTransaction();
    procedure Commit();
    procedure Rollback();

    procedure Save(AObject: THadeObject);
    procedure Read(AObject: THadeObject;AFetchMode:TFetchMode = fcLazy);

    //objectlists
    procedure Read(const AObjectList:THadeObjectList;AFetchMode:TFetchMode = fcLazy);
    procedure ApplyUpdate(const AObjectList:THadeObjectList;
      AUpdateMode:TUpdateMode = upStopError);

    {procedure InsertRow(const AClassName:string;ACriteria:THadeCriteria);
    procedure UpdateRow(const AClassName:string;ACriteria:THadeCriteria);
    procedure DeleteRow(const AClassName:string;ACriteria:THadeCriteria);}

    function RecordsCount(ATableName:string):ptrUint;
    procedure ExecuteScripts(const AFileName:string);

    constructor Create(AConnectionName: string = '');
    destructor Destroy; override;
  end;

implementation
uses
  hdopfmanager;
{ THadeDatabase }

procedure THadeDatabase.RaiseError(const AMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self)
  else
    raise EHadeDatabaseException.Create(AMsg);
end;

procedure THadeDatabase.SetOnBeforeRead(AValue: TNotifyEvent);
begin
  if FOnBeforeRead=AValue then Exit;
  FOnBeforeRead:=AValue;
end;

procedure THadeDatabase.SetOnBeforeSave(AValue: TNotifyEvent);
begin
  if FOnBeforeSave=AValue then Exit;
  FOnBeforeSave:=AValue;
end;

procedure THadeDatabase.SetOnCommit(AValue: TNotifyEvent);
begin
  if FOnCommit=AValue then Exit;
  FOnCommit:=AValue;
end;

procedure THadeDatabase.SetOnConnect(AValue: TNotifyEvent);
begin
  if FOnConnect=AValue then Exit;
  FOnConnect:=AValue;
end;

procedure THadeDatabase.SetOnConnectError(AValue: TNotifyEvent);
begin
  if FOnConnectError=AValue then Exit;
  FOnConnectError:=AValue;
end;

procedure THadeDatabase.SetOnDisconnect(AValue: TNotifyEvent);
begin
  if FOnDisconnect=AValue then Exit;
  FOnDisconnect:=AValue;
end;

procedure THadeDatabase.SetOnError(AValue: TNotifyEvent);
begin
  if FOnError=AValue then Exit;
  FOnError:=AValue;
end;

procedure THadeDatabase.InitConnection;
begin
  FConnection := GHadeConnectionFactory.ObtainConnection(FConnectionName);
  FConnection.OnConnect:= Self.FOnConnect;
  FConnection.OnDisconnect:= Self.FOnDisconnect;
  FConnection.OnConnectError:= Self.FOnConnectError;
  FConnection.Connect;
end;

procedure THadeDatabase.ReturnConnection;
begin
  GHadeConnectionFactory.ReturnConnection(FConnection);
end;

procedure THadeDatabase.StartTransaction;
begin
  fConnection.StartTransaction();
end;

procedure THadeDatabase.Commit;
begin
  fConnection.Commit();
  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

procedure THadeDatabase.Rollback;
begin
  fConnection.Rollback();
end;

procedure THadeDatabase.Save(AObject: THadeObject);
begin
  Self.StartTransaction();
  try
    if Assigned(FOnBeforeSave) then
      FOnBeforeSave(Self);
    FImplementor.Save(AObject);
    Self.Commit();
  except
    on E:Exception do
    begin
      Self.Rollback();
      Self.RaiseError(E.Message);
    end;
  end;
end;

procedure THadeDatabase.Read(AObject: THadeObject; AFetchMode: TFetchMode);
begin
  if Assigned(FOnBeforeRead) then
    FOnBeforeRead(Self);

  FImplementor.read(AObject,AFetchMode);
end;

procedure THadeDatabase.Read(const AObjectList: THadeObjectList;
  AFetchMode: TFetchMode);
begin
  FImplementor.read(AObjectList,AObjectList.Criteria,AFetchMode);
end;

procedure THadeDatabase.ApplyUpdate(const AObjectList: THadeObjectList;
  AUpdateMode: TUpdateMode);
begin
  Self.StartTransaction();
  try
    Self.FImplementor.ApplyUpdate(AObjectList);
  Except
    on E:Exception do
    begin
      if AUpdateMode = upStopError then
      begin
        Self.Rollback();
        Self.RaiseError(E.Message);
      end;
    end;
  end;

  Self.Commit();
end;

function THadeDatabase.RecordsCount(ATableName: string): ptrUint;
var
  pk: String;
begin
  Result:= 0;
  pk:= GHadeOPFManager.PersistenceMapper.FindTableMap(ATableName).getPK.ColumnName;
  FQuery.SQL.Text:='SELECT COUNT('+pk+') as total FROM '+ATableName;
  FQuery.Open;
  Result:= FQuery.Fields.FieldByName('total').AsInteger;

  FQuery.Close;
  FQuery.SQL.Clear;
end;

procedure THadeDatabase.ExecuteScripts(const AFileName: string);
begin
  FQuery.ExecuteScriptFile(AFileName);
end;

constructor THadeDatabase.Create(AConnectionName: string);
begin
  FConnectionName := AConnectionName;
  Self.InitConnection;
  FQuery := THadeQuery.Create(FConnection);
  FImplementor:= THadeImplementor.Create(FConnection);
end;

destructor THadeDatabase.Destroy;
begin
  FQuery.Free;
  FImplementor.Free;
  Self.ReturnConnection;
  inherited Destroy;
end;

end.
