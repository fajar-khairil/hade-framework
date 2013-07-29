unit hdconnection;
{ TODO 2 -oFajar -cDatabaseConnection : -get all table names
-get all field names by table }
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdbroker,
  hdconnectionintf,
  hdbase,
  contnrs,
  hdmapper;

type
  EHadeConnectionException = class(EHadeException)
  end;

  { THadeConnection }

  THadeConnection = class(THadeBaseObject)
  private
    fConnection: IHadeDatabaseConnection;
    fBroker: THadeBroker;
    fConnectionName: string;
    FOnConnect: TNotifyEvent;
    FOnConnectError: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;

    function getDatabase: string;
    function getHandle: Pointer;
    function getHost: string;
    function GetParams: TStrings;
    function getPassword: string;
    function getUsername: string;
    procedure setDatabase(AValue: string);
    procedure setHost(AValue: string);
    procedure SetOnConnect(AValue: TNotifyEvent);
    procedure SetOnConnectError(AValue: TNotifyEvent);
    procedure SetOnDisconnect(AValue: TNotifyEvent);
    procedure setPassword(AValue: string);
    procedure setUsername(AValue: string);

    procedure RaiseError(const AMsg: string);
  public
    property OnConnect:TNotifyEvent read FOnConnect write SetOnConnect;
    property OnDisconnect:TNotifyEvent read FOnDisconnect write SetOnDisconnect;
    property OnConnectError:TNotifyEvent read FOnConnectError write SetOnConnectError;

    procedure Connect();
    procedure Disconnect();
    procedure ExecuteDirect(const ASql: string);
    procedure StartTransaction();
    procedure Commit();
    procedure Rollback();

    property Handle: Pointer read getHandle;
    property Broker: THadeBroker read fBroker;
    property Username: string read getUsername write setUsername;
    property Host: string read getHost write setHost;
    property Database: string read getDatabase write setDatabase;
    property Password: string read getPassword write setPassword;
    property ConnectionName: string read fConnectionName;

    property Params : TStrings read GetParams;

    constructor Create(AConnectionName: string);
    destructor Destroy(); override;
  end;

  { THadeConnectionFactory }

  THadeConnectionFactory = class(THadeBaseObject)
  private
    fMaxCount: ptrUint;
    fList: TFpObjectList;
    fPurge: ptrUint;
    procedure setPurge(AValue: ptrUint);
  public
    class function getConnection(ABroker: THadeBroker): IHadeDatabaseConnection;

    function ObtainConnection(AConnectionName: string): THadeConnection;
    function ReturnConnection(AConnection: THadeConnection): ptrUint;
    function Count: ptrUint;
    procedure Clear;
    property purgeTime: ptrUint read fPurge write setPurge;

    constructor Create(AMaxCount: ptrUInt = 10);
    destructor Destroy; override;
  end;

var
  GHadeConnectionFactory: THadeConnectionFactory = nil;

implementation

uses
  hdsqldbbase,
  DB,
  hdopfmanager;
{ THadeConnection }

function THadeConnection.getDatabase: string;
begin
  Result := fConnection.getDatabase;
end;

function THadeConnection.getHandle: Pointer;
begin
  Result := fConnection.getHandle;
end;

function THadeConnection.getHost: string;
begin
  Result := fConnection.getHost;
end;

function THadeConnection.GetParams: TStrings;
begin
  Result:= FConnection.Params;
end;

function THadeConnection.getPassword: string;
begin
  Result := fConnection.getPassword;
end;

function THadeConnection.getUsername: string;
begin
  Result := fConnection.getUsername;
end;

procedure THadeConnection.setDatabase(AValue: string);
begin
  fConnection.setDatabase(AValue);
end;

procedure THadeConnection.setHost(AValue: string);
begin
  fConnection.setHost(AValue);
end;

procedure THadeConnection.SetOnConnect(AValue: TNotifyEvent);
begin
  if FOnConnect=AValue then Exit;
  FOnConnect:=AValue;
end;

procedure THadeConnection.SetOnConnectError(AValue: TNotifyEvent);
begin
  if FOnConnectError=AValue then Exit;
  FOnConnectError:=AValue;
end;

procedure THadeConnection.SetOnDisconnect(AValue: TNotifyEvent);
begin
  if FOnDisconnect=AValue then Exit;
  FOnDisconnect:=AValue;
end;

procedure THadeConnection.setPassword(AValue: string);
begin
  fConnection.setPassword(AValue);
end;

procedure THadeConnection.setUsername(AValue: string);
begin
  fConnection.setUsername(AValue);
end;

procedure THadeConnection.RaiseError(const AMsg: string);
begin
  raise EHadeConnectionException.Create(AMsg);
end;

procedure THadeConnection.Connect;
begin
  try
    fConnection.Connect();
    if Assigned(FOnConnect) then FOnConnect(self);
  except
    on E: EDatabaseError do
    begin
      if Assigned(FOnConnectError) then FOnConnectError(self);
      RaiseError(E.Message);
    end;
  end;
end;

procedure THadeConnection.Disconnect;
begin
  try
    fConnection.Disconnect();
    if Assigned(FOnDisconnect) then FOnDisconnect(self);
  except
    on E: EDatabaseError do
      RaiseError(E.Message);
  end;
end;

procedure THadeConnection.ExecuteDirect(const ASql: string);
begin
  try
    fConnection.ExecuteDirect(ASql);
  except
    on E: EDatabaseError do
      RaiseError(E.Message);
  end;
end;

procedure THadeConnection.StartTransaction;
begin
  try
    fConnection.StartTransaction();
  except
    on E: EDatabaseError do
      RaiseError(E.Message);
  end;
end;

procedure THadeConnection.Commit;
begin
  try
    fConnection.Commit();
  except
    on E: EDatabaseError do
      RaiseError(E.Message);
  end;
end;

procedure THadeConnection.Rollback;
begin
  try
    fConnection.Rollback();
  except
    on E: EDatabaseError do
      RaiseError(E.Message);
  end;
end;

constructor THadeConnection.Create(AConnectionName: string);
var
  con: THadeDatabaseMapper;
begin
  inherited Create;
  fConnectionName := AConnectionName;
  con := GHadeOPFManager.PersistenceMapper.Connections.Find(fConnectionName);
  fBroker := con.Broker;
  fConnection := GHadeConnectionFactory.getConnection(fBroker);
  self.Host := con.host;
  self.Database := con.Database;
  self.Username := con.Username;
  self.Password := con.Password;
end;

destructor THadeConnection.Destroy;
begin
  self.Disconnect();
  inherited Destroy;
end;

{ THadeConnectionFactory }

procedure THadeConnectionFactory.setPurge(AValue: ptrUint);
begin
  if fPurge = AValue then
    Exit;
  fPurge := AValue;
end;

class function THadeConnectionFactory.getConnection(ABroker: THadeBroker):
IHadeDatabaseConnection;
begin
  case ABroker of
    SqlDBFirebird: Result := THadeSqlDBFirebird.Create();
    SqlDBMySQL: Result := THadeSqlDBMySQL.Create();
    SqlDBSqLite: Result := THadeSqlDBSQlite.Create();
    SqlDBPostgree: Result := THadeSqlDBPostgree.Create();
    SqlDBOracle: Result := THadeSqlDBOracle.Create();
    else
      raise EHadeConnectionException.Create('Wrong Broker.');
  end;
end;

function THadeConnectionFactory.ObtainConnection(AConnectionName: string
  ): THadeConnection;
var
  iloop: integer;
  con: THadeDatabaseMapper;
begin
  Result := nil;

  for iloop := 0 to pred(Count) do
  begin
    if lowercase(THadeConnection(fList.Items[iloop]).ConnectionName) =
      lowercase(AConnectionName) then
    begin
      Result := THadeConnection(fList.Extract(fList.Items[iloop]));
      break;
    end;
  end;

  if not Assigned(Result) then
  begin
    con := GHadeOPFManager.PersistenceMapper.Connections.Find(AConnectionName);
    Result := THadeConnection.Create(AConnectionName);
    Result.Host := con.Host;
    Result.Database := con.Database;
    Result.Username := con.Username;
    Result.Password := con.Password;
    result.Params.Text:= Con.Params.Text;
  end;
end;

function THadeConnectionFactory.ReturnConnection(AConnection: THadeConnection): ptrUint;
begin
  if Count >= FMaxCount then
    Self.Clear;

  AConnection.Disconnect();
  Result := FList.Add(AConnection);
end;

function THadeConnectionFactory.Count: ptrUint;
begin
    Result := fList.Count;
end;

procedure THadeConnectionFactory.Clear;
var
  iloop: Integer;
begin
  for iloop:=0 to pred(count) do
    THadeConnection(FList.Items[iloop]).Disconnect();
  FList.Clear;
end;

constructor THadeConnectionFactory.Create(AMaxCount: ptrUInt);
begin
  fMaxCount := AMaxCount;
  fList := TFpObjectList.Create(True);
  fList.Capacity := fMaxCount;
  fPurge := (60 * 5) * 1000;//5minute
end;

destructor THadeConnectionFactory.Destroy;
begin
  Self.Clear;
  FreeAndNil(fList);
  inherited Destroy;
end;

initialization
  if not Assigned(GHadeConnectionFactory) then
    GHadeConnectionFactory := THadeConnectionFactory.Create;

finalization
  if Assigned(GHadeConnectionFactory) then
    FreeAndNil(GHadeConnectionFactory);
end.
