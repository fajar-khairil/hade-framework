unit hdsqldbquery;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdqueryintf,
  sqldb,
  DB,
  fpjson;

type

  { THadeSQLDbQuery }
  { SQLDB Implementation }
  THadeSQLDbQuery = class(TInterfacedObject, IHadeQuery)
  private
    function GetBof: boolean;
    function GetEof: boolean;
  protected
    FCon: TDatabase;
    FQuery: TSQLQuery;
    procedure SetSQL(AValue: TStringList);
    function getFields: TFields;
    function GetHandle: pointer;
    function getParams: TParams;
    function GetSQL: TStringList;
  public
    property Fields: TFields read getFields;
    property Params: TParams read getParams;
    property SQL: TStringList read GetSQL write SetSQL;
    property Handle: pointer read GetHandle;

    procedure Prepare;
    procedure UnPrepare;
    function RowsAffected: TRowsCount;
    procedure Next;
    procedure prior;
    procedure First;
    procedure last;
    procedure Open;
    procedure Close;
    procedure ExecSQL;

    property EOF:boolean read GetEof;
    property BOF:boolean read GetBof;

    procedure ExecuteScriptFile(AFileName: string; ATerminator: char = ';');
    //json
    procedure AsJson(out AJsonArray: TJSONArray);
    function ToJsonString: string;

    constructor Create(AConnectionHandle: Pointer); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ THadeSQLDbQuery }

function THadeSQLDbQuery.GetHandle: pointer;
begin
  Result := FQuery;
end;

procedure THadeSQLDbQuery.Prepare;
begin
  FQuery.Prepare;
end;

procedure THadeSQLDbQuery.UnPrepare;
begin
  FQuery.UnPrepare;
end;

function THadeSQLDbQuery.RowsAffected: TRowsCount;
begin
  Result := FQuery.RowsAffected;
end;

procedure THadeSQLDbQuery.Next;
begin
  FQuery.Next;
end;

procedure THadeSQLDbQuery.prior;
begin
  FQuery.prior;
end;

procedure THadeSQLDbQuery.First;
begin
  FQuery.First;
end;

procedure THadeSQLDbQuery.last;
begin
  FQuery.Last;
end;

procedure THadeSQLDbQuery.Open;
begin
  FQuery.Open;
end;

procedure THadeSQLDbQuery.Close;
begin
  FQuery.Close;
end;

procedure THadeSQLDbQuery.ExecSQL;
begin
  FQuery.ExecSQL;
end;

function THadeSQLDbQuery.getParams: TParams;
begin
  Result := FQuery.Params;
end;

function THadeSQLDbQuery.GetSQL: TStringList;
begin
  Result := FQUery.SQL;
end;

function THadeSQLDbQuery.GetBof: boolean;
begin
  Result:= FQuery.EOF;
end;

function THadeSQLDbQuery.GetEof: boolean;
begin
 Result := FQuery.BOF;
end;

procedure THadeSQLDbQuery.SetSQL(AValue: TStringList);
begin
  FQuery.SQL := AValue;
end;

function THadeSQLDbQuery.getFields: TFields;
begin
  Result := FQuery.Fields;
end;

procedure THadeSQLDbQuery.ExecuteScriptFile(AFileName: string; ATerminator: char);
var
  FSCript: TSQLScript;
  sqlFile: TStringList;
begin
  sqlFile := TStringList.Create;
  FSCript := TSQLScript.Create(nil);
  FScript.DataBase := FQuery.DataBase;
  FScript.Transaction := FQuery.Transaction;
  FScript.Terminator := ATerminator;
  try
    sqlfile.LoadFromFile(AFileName);
    FScript.Script.Text := sqlfile.Text;
    FScript.ExecuteScript;
  finally
    FScript.Free;
    sqlfile.Free;
  end;
end;

procedure THadeSQLDbQuery.AsJson(out AJsonArray: TJSONArray);
var
  cjson: TJSONObject;
  iloop: integer;
  AField: TField;
begin
  AJsonArray := TJsonArray.Create;
  FQuery.First;
  while not FQuery.EOF do
  begin
    cjson := TJSonObject.Create;
    for iloop := 0 to pred(Self.Fields.Count) do
    begin
      AField := FQuery.Fields.Fields[iloop];
      if AField.DataType in [ftString, ftWideString, ftVariant,
        ftFixedWideChar, ftUnknown, ftMemo] then
        cjson.Add(Self.Fields.Fields[iloop].FieldName, AField.AsString);

      if AField.DataType in [ftSmallInt, ftInteger, ftAutoInc] then
        cjson.Add(AField.FieldName, AField.AsInteger);

      if AField.DataType in [ftFloat, ftCurrency] then
        cjson.Add(AField.FieldName, AField.AsFloat);

      if AField.DataType in [ftDate, ftTime, ftDateTime] then
        cjson.Add(AField.FieldName, AField.AsDateTime);

      if AField.DataType in [ftBoolean] then
        cjson.Add(AField.FieldName, AField.AsBoolean);
    end;
    Self.Next;
    AJsonArray.Add(cjson);
  end;
end;

function THadeSQLDbQuery.ToJsonString: string;
var
  ajson: TJSONArray;
begin
  try
    Self.AsJson(ajson);
    Result := aJson.AsJSON;
  finally
    ajson.Free;
  end;
end;

constructor THadeSQLDbQuery.Create(AConnectionHandle: Pointer);
begin
  FCon := TDatabase(AConnectionHandle);
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FCon;
  FQuery.Transaction := FCon.Transactions[pred(FCon.TransactionCount)];
end;

destructor THadeSQLDbQuery.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

end.
