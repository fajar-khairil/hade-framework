unit hdquery;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdbase,
  hdqueryintf,
  hdconnection,
  DB,
  fpjson;

type

  { EHadeQuery }

  EHadeQueryException = class(EHadeException);

  { THadeQuery }

  THadeQuery = class(THadeBaseObject)
  private
    FConnectionName: string;
    fQuery: IHadeQuery;
    FConnection:THadeConnection;
    function GetBof: boolean;
    function GetEof: boolean;
  protected
    procedure setSQL(AValue: TStringList);
    function getSQL: TStringList;
    function getHandle: pointer;
    function getFields: TFields;
    function getParams: TParams;
    procedure RaiseError(const AMsg: string);
  public
    procedure ExecSQL;
    procedure Open;
    procedure Close;

    procedure Next;
    procedure prior;
    procedure First;
    procedure last;

    procedure Prepare;
    procedure UnPrepare;

    property Handle: pointer read getHandle;
    property Fields:TFields read getFields;
    property Params:TParams read getParams;
    property SQL: TStringList read getSQL write setSQL;
    procedure AsJson(out AJsonArray:TJSONArray);
    function ToJsonString:string;

    property EOF:boolean read GetEof;
    property BOF:boolean read GetBof;

    function GetLastInsertID:int64;

    procedure ExecuteScriptFile(AFileName:string;ATerminator:char = ';');

    constructor Create(AConnection: THadeConnection);
  end;

implementation

uses
  hdbroker,
  hdsqldbquery;

{ THadeQuery }

procedure THadeQuery.RaiseError(const AMsg: string);
begin
  //dbugintf.SendDebug(AMsg);
  raise EHadeQueryException.Create(AMsg);
end;

function THadeQuery.getFields: TFields;
begin
  Result:= FQuery.Fields;
end;

function THadeQuery.getParams: TParams;
begin
  Result:= FQuery.Params;
end;

function THadeQuery.GetBof: boolean;
begin
  Result:= FQuery.EOF;
end;

function THadeQuery.GetEof: boolean;
begin
  Result:= FQuery.BOF;
end;

procedure THadeQuery.setSQL(AValue: TStringList);
begin
  fQuery.SQL := AValue;
end;

function THadeQuery.getSQL: TStringList;
begin
  Result := fQuery.SQL;
end;

procedure THadeQuery.ExecSQL;
begin
  try
    fQuery.ExecSQL;
  except
    on E: Exception do
      RaiseError(E.Message);
  end;
end;

procedure THadeQuery.Open;
begin
  try
    fQuery.Open;
  except
    on E: Exception do
      RaiseError(E.Message);
  end;
end;

procedure THadeQuery.Close;
begin
  try
    fQuery.Close;
  except
    on E: Exception do
      RaiseError(E.Message);
  end;
end;

procedure THadeQuery.Next;
begin
  fQuery.Next;
end;

procedure THadeQuery.prior;
begin
  fQuery.prior;
end;

procedure THadeQuery.First;
begin
  fQuery.First;
end;

procedure THadeQuery.last;
begin
  fQuery.last;
end;

procedure THadeQuery.Prepare;
begin
  FQuery.Prepare;
end;

procedure THadeQuery.UnPrepare;
begin
  FQuery.UnPrepare;
end;

procedure THadeQuery.AsJson(out AJsonArray: TJSONArray);
begin
  FQuery.AsJson(AJsonArray);
end;

function THadeQuery.ToJsonString: string;
begin
  Result:= FQUery.ToJsonString;
end;

function THadeQuery.GetLastInsertID: int64;
begin
  Result:= 0;
  if not (FConnection.Broker in [SQLDBSqlite,SQLDBMySQL]) then Exit;

  FQuery.SQL.Clear;
  case FConnection.Broker of
    SQLDBSqlite: FQuery.SQL.Add('SELECT last_insert_rowid() as id');
  end;

  FQuery.Open;
  Result:= FQuery.Fields.FieldByName('id').AsInteger;
end;

procedure THadeQuery.ExecuteScriptFile(AFileName: string; ATerminator: char);
begin
  FConnection.StartTransaction();
  try
    FQuery.ExecuteScriptFile(AFileName,ATerminator);
    FConnection.Commit();
  except
    on E:Exception do
    begin
      FConnection.Rollback();
      raise e;
    end;
  end;
end;

function THadeQuery.getHandle: pointer;
begin
  Result := fQuery.getHandle;
end;

constructor THadeQuery.Create(AConnection: THadeConnection);
begin
  FConnection:= AConnection;
  if hdbroker.isSQLDb(FConnection.Broker) then
  begin
    FQuery:= THadeSQLDbQuery.Create(FConnection.Handle);
  end
  else
    RaiseError('Wrong or not Supported Broker.');

  FConnectionName := FConnection.ConnectionName;
end;

end.

