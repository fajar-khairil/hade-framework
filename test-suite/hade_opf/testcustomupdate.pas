unit testcustomupdate;

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

  { TTesCustomQueryUpdate }

  TTesCustomQueryUpdate= class(TTestCase)
  protected
    FUpdate: THadeQueryBuilderUpdate;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUpdate;
  end;

implementation
uses
  hdbroker;

procedure TTesCustomQueryUpdate.TestUpdate;
var
  expect: String;
begin
  expect:= 'UPDATE person SET first_name = fajar,last_name = khairil,age = 23 WHERE id = 1';
  FUpdate.Update('person','first_name,last_name,age');
  FUpdate.Values('fajar,khairil,23');
  FUpdate.Criteria.Equal('id','1');

  Self.AssertEquals( UpperCase(Expect),UpperCase(FUpdate.GetClause) );
end;

procedure TTesCustomQueryUpdate.SetUp;
begin
  FUpdate:= THadeQueryBuilderFactory.GetUpdateBuilder(SQLDBSqlite);
end;

procedure TTesCustomQueryUpdate.TearDown;
begin
 FUpdate.Free;
end;

initialization

  RegisterTest(TTesCustomQueryUpdate);
end.

