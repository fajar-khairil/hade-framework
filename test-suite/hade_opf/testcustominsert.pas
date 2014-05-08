unit testcustominsert;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry;

type

  { TTestCustomQueryInsert }

  TTestCustomQueryInsert= class(TTestCase)
  protected
    //procedure SetUp; override;
    //procedure TearDown; override;
  published
    procedure TestInsertSQLite;
    procedure TestInsertFirebird;
  end;

implementation
uses
  hdbroker,
  hdquerybuilder;

procedure TTestCustomQueryInsert.TestInsertSQLite;
var
  FInsert: THadeQueryBuilderInsert;
  Expect: String;
begin
  FInsert:= THadeQueryBuilderFactory.GetInsertBuilder(SQLDBSQlite);
  Expect:= 'INSERT INTO PERSON (first_name,last_name,age) '+
           'VALUES(fajar,khairil,23)';
  try
    FInsert.Insert('person','first_name,last_name,age');
    FInsert.Values('fajar,khairil,23');

    Self.AssertEquals( Uppercase(Expect),UpperCase(FInsert.GetClause) );
  finally
    FInsert.Free;
  end;
end;

procedure TTestCustomQueryInsert.TestInsertFirebird;
var
  FInsert: THadeQueryBuilderInsert;
  Expect: String;
begin
  FInsert:= THadeQueryBuilderFactory.GetInsertBuilder(SQLDBFirebird);
  Expect:= 'INSERT INTO PERSON (first_name,last_name,age) '+
           'VALUES(fajar,khairil,23) RETURNING id';
  try
    FInsert.Insert('person','first_name,last_name,age');
    FInsert.Values('fajar,khairil,23');

    Self.AssertEquals( Uppercase(Expect),UpperCase(FInsert.GetClause) );
  finally
    FInsert.Free;
  end;
end;

initialization

  RegisterTest(TTestCustomQueryInsert);
end.

