unit hdsqldbbase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdconnectionintf,
  sqldb;

type

  { THadeSqlDBBaseConnection }

  THadeSqlDBBaseConnection = class(TInterfacedObject, IHadeDatabaseConnection)
  protected
    fConnection: TSQLConnection;
    fTransaction: TSQLTransaction;

    function GetParams: TStrings;
    function getDatabase: string;
    function getHost: string;
    function getPassword: string;
    function getUsername: string;
    procedure setDatabase(AValue: string);
    procedure setHost(AValue: string);
    procedure setPassword(AValue: string);
    procedure setUsername(AValue: string);
  public
    procedure Connect();
    procedure Disconnect();
    procedure ExecuteDirect(const ASql: string);
    procedure StartTransaction();
    procedure Commit();
    procedure Rollback();
    function getHandle: Pointer;

    property Username: string read getUsername write setUsername;
    property Host: string read getHost write setHost;
    property Database: string read getDatabase write setDatabase;
    property Password: string read getPassword write setPassword;

    property Params : TStrings read GetParams;

    constructor Create(); virtual;
    destructor Destroy(); override;
  end;

  { THadeSqlDBFirebird }

  THadeSqlDBFirebird = class(THadeSqlDBBaseConnection)
  public
    constructor Create(); override;
  end;

  { THadeSqlDBMySQL }

  THadeSqlDBMySQL = class(THadeSqlDBBaseConnection)
  public
    constructor Create(); override;
  end;

  { THadeSqlDBSqlite }

  THadeSqlDBSqlite = class(THadeSqlDBBaseConnection)
  public
    constructor Create(); override;
  end;

  { THadeSqlDBPostgree }

  THadeSqlDBPostgree = class(THadeSqlDBBaseConnection)
  public
    constructor Create(); override;
  end;

  { THadeSqlDBOracle }

  THadeSqlDBOracle = class(THadeSqlDBBaseConnection)
  public
    constructor Create(); override;
  end;

implementation

uses
  ibconnection,
  mysql55conn,
  pqconnection,
  oracleconnection,
  sqlite3conn;

{ THadeSqlDBOracle }

constructor THadeSqlDBOracle.Create;
begin
  inherited Create;
  fConnection := TOracleConnection.Create(nil);
  fTransaction.DataBase := fConnection;
end;

{ THadeSqlDBPostgree }

constructor THadeSqlDBPostgree.Create;
begin
  inherited Create;
  fConnection := TPQConnection.Create(nil);
  fTransaction.DataBase := fConnection;
end;

{ THadeSqlDBMySQL }

constructor THadeSqlDBMySQL.Create;
begin
  inherited Create;
  fConnection := TMySQL55Connection.Create(nil);
  fTransaction.DataBase := fConnection;
end;

{ THadeSqlDBSqlite }

constructor THadeSqlDBSqlite.Create;
begin
  inherited Create;
  fConnection := TSQLite3Connection.Create(nil);
  fTransaction.DataBase := fConnection;
end;

{ THadeSqlDBFirebird }

constructor THadeSqlDBFirebird.Create;
begin
  inherited Create;
  fConnection := TIBConnection.Create(nil);
  fTransaction.DataBase := fConnection;
end;

{ THadeSqlDBBaseConnection }

function THadeSqlDBBaseConnection.GetParams: TStrings;
begin
  Result:= FConnection.Params;
end;

function THadeSqlDBBaseConnection.getDatabase: string;
begin
  Result := fConnection.DatabaseName;
end;

function THadeSqlDBBaseConnection.getHost: string;
begin
  Result := fConnection.HostName;
end;

function THadeSqlDBBaseConnection.getPassword: string;
begin
  Result := fConnection.Password;
end;

function THadeSqlDBBaseConnection.getUsername: string;
begin
  Result := fConnection.UserName;
end;

procedure THadeSqlDBBaseConnection.setDatabase(AValue: string);
begin
  fConnection.DatabaseName := trim(AValue);
end;

procedure THadeSqlDBBaseConnection.setHost(AValue: string);
begin
  fConnection.HostName := trim(AValue);
end;

procedure THadeSqlDBBaseConnection.setPassword(AValue: string);
begin
  fConnection.Password := trim(AValue);
end;

procedure THadeSqlDBBaseConnection.setUsername(AValue: string);
begin
  fConnection.UserName := trim(AValue);
end;

procedure THadeSqlDBBaseConnection.Connect;
begin
  fConnection.Connected := True;
end;

procedure THadeSqlDBBaseConnection.Disconnect;
begin
  fConnection.Connected := False;
end;

procedure THadeSqlDBBaseConnection.ExecuteDirect(const ASql: string);
begin
  fConnection.ExecuteDirect(Asql);
end;

procedure THadeSqlDBBaseConnection.StartTransaction;
begin
  if not FTransaction.Active then
  begin
    FTransaction.StartTransaction;
  end;
end;

procedure THadeSqlDBBaseConnection.Commit;
begin
  if FTransaction.Active then
  begin
    FTransaction.Commit;
  end;
end;

procedure THadeSqlDBBaseConnection.Rollback;
begin
  if FTransaction.Active then
  begin
    FTransaction.Rollback;
  end;
end;

function THadeSqlDBBaseConnection.getHandle: Pointer;
begin
  Result := fConnection;
end;

constructor THadeSqlDBBaseConnection.Create;
begin
  fTransaction := TSQLTransaction.Create(nil);
  fTransaction.Active:= False;
end;

destructor THadeSqlDBBaseConnection.Destroy;
begin
  FreeAndNil(fConnection);
  FreeAndNil(fTransaction);
  inherited Destroy();
end;

end.
