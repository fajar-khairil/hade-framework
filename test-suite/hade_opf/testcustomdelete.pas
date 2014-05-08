unit testcustomdelete;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  hdquerybuilder;

type

  { TTestCustomQueryDelete }

  TTestCustomQueryDelete= class(TTestCase)
  protected
    FDelete:THadeQueryBuilderDelete;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDelete;
  end;

implementation
uses
  hdbroker;

procedure TTestCustomQueryDelete.TestDelete;
var
  Expect: String;
begin
  Expect:= 'DELETE FROM PERSON WHERE ID = 2';
  FDelete.Delete('person');
  FDelete.Criteria.Equal('id','2');

  Self.AssertEquals( UpperCase(Expect),UpperCase(FDelete.GetClause) );
end;

procedure TTestCustomQueryDelete.SetUp;
begin
  FDelete:= THadeQueryBuilderFactory.GetDeleteBuilder(SQLDBSqlite);
end;

procedure TTestCustomQueryDelete.TearDown;
begin
  FDelete.Free;
end;

initialization

  RegisterTest(TTestCustomQueryDelete);
end.

