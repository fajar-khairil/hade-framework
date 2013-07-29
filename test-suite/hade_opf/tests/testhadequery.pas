unit testhadequery;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  hdquery,
  hdconnection;

type

  { TTestHadeQuery }

  TTestHadeQuery= class(TTestCase)
  protected
    FQuery : THadeQuery;
    FCon:THadeConnection;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsJson;
    procedure TestParams;
    procedure TestFields;
  end;

implementation
uses
  testinit,
  fpjson;

procedure TTestHadeQuery.TestAsJson;
var
  fjson: TJSONArray;
begin
  try
    FQuery.SQL.text :='SELECT * FROM person WHERE id = 1';
    FQuery.Open;
    FQuery.AsJson(fjson);
    self.AssertEquals(1,fjson.Count);
    self.AssertEquals('Fajar',TJSONObject(fjson.Items[0]).Strings['first_name']);
  finally
    FQuery.Close;
    if Assigned(fjson) then
      FreeAndNil(fjson);
  end;
end;

procedure TTestHadeQuery.TestParams;
begin
  FQuery.SQL.Text:= 'SELECT * FROM PERSON WHERE ID = :ID';
  FQuery.Params.ParamByName('ID').AsInteger:= 5;

  self.AssertEquals(5,FQuery.Params.ParamByName('ID').AsInteger);
end;

procedure TTestHadeQuery.TestFields;
begin
  FQuery.SQL.Text:= 'SELECT * FROM PERSON WHERE ID = :ID';
  FQuery.Params.ParamByName('ID').AsInteger:= 1;
  FQuery.Open;

  Self.AssertEquals('Fajar',FQuery.Fields.FieldByName('first_name').AsString );
end;

procedure TTestHadeQuery.SetUp;
begin
  testinit.initMapper;
  FCon := GHadeConnectionFactory.ObtainConnection('sqlite');
  FQuery := THadeQuery.Create(FCon);

  try
    FQuery.ExecuteScriptFile('/home/fajar/bin/person.sql');
  except
    on E:Exception do
    begin
      WriteLn(E.Message);
      clearMapper;
      FQuery.Free;
    end;
  end;
end;

procedure TTestHadeQuery.TearDown;
begin
  testinit.clearMapper;
  FQuery.Free;
  FCon.Free;
end;

initialization

  RegisterTest(TTestHadeQuery);
end.

