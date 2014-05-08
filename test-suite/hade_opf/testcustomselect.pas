unit testcustomselect;

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

  { TTestCustomSelect }

  TTestCustomSelect= class(TTestCase)
  protected
    FSelect:THadeQueryBuilderSelect;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleSelect;
    procedure TestJoin;
    procedure TestHaving;
    procedure TestLimitSQLite;
    procedure TestLimitFirebird;
    procedure TestOrderBy;
  end;

implementation
uses
  hdbroker;

procedure TTestCustomSelect.SetUp;
begin
  FSelect:= THadeQueryBuilderFactory.GetSelectBuilder(SQLDBSqlite);
end;

procedure TTestCustomSelect.TearDown;
begin
  FSelect.Free;
end;

procedure TTestCustomSelect.TestSimpleSelect;
var
  Expect: String;
begin
  Expect:='SELECT first_name,last_name,age FROM person WHERE age = 30';
  FSelect.Select('first_name,last_name,age');
  FSelect.From('person');
  FSelect.Criteria.Equal('age','30');

  Self.AssertEquals(Expect,FSelect.GetClause);
end;

procedure TTestCustomSelect.TestJoin;
var
  Expect: String;
begin
  Expect := 'SELECT first_name,last_name,age FROM person '+
            'INNER JOIN company on(person.company_id = company.id) WHERE age = 30';
  FSelect.Select('first_name,last_name,age');
  FSelect.From('person');
  FSelect.Join('company','person.company_id','company.id');
  FSelect.Criteria.Equal('age','30');

  Self.AssertEquals( Uppercase(Expect),uppercase(FSelect.GetClause) );
end;

procedure TTestCustomSelect.TestHaving;
var
  Expect: String;
begin
  Expect:='SELECT first_name,last_name,age FROM person '+
          'WHERE age = 30 GROUP BY FIRST_NAME HAVING COUNT(age) > 21';
  FSelect.Select('first_name,last_name,age');
  FSelect.From('person');
  FSelect.Criteria.Equal('age','30');
  FSelect.Having('COUNT(age)','>','21');
  FSelect.GroupBy('first_name');

  Self.AssertEquals( Uppercase(Expect),uppercase(FSelect.GetClause) );
end;

procedure TTestCustomSelect.TestLimitSQLite;
var
  Expect: String;
begin
  Expect:='SELECT first_name,last_name,age FROM person WHERE age = 30 LIMIT 0,10';
  FSelect.Select('first_name,last_name,age');
  FSelect.From('person');
  FSelect.Criteria.Equal('age','30');
  FSelect.Limit(0,10);

  Self.AssertEquals(Expect,FSelect.GetClause);
end;

procedure TTestCustomSelect.TestLimitFirebird;
var
  Expect: String;
  FbSelect: THadeQueryBuilderSelect;
begin
  Expect:='SELECT first_name,last_name,age FROM person WHERE age = 30 ROWS 1 TO 10';
  FbSelect:= THadeQueryBuilderFactory.GetSelectBuilder(SQLDBFirebird);

  try
    FbSelect.Select('first_name,last_name,age');
    FbSelect.From('person');
    FbSelect.Criteria.Equal('age','30');
    FbSelect.Limit(0,10);
    Self.AssertEquals(Expect,FbSelect.GetClause);
  finally
    FBSelect.Free;
  end;
end;

procedure TTestCustomSelect.TestOrderBy;
var
  Expect: String;
begin
  Expect:='SELECT first_name,last_name,age FROM person '+
           'WHERE age = 30 ORDER BY first_name ASC LIMIT 0,10';
  FSelect.Select('first_name,last_name,age');
  FSelect.From('person');
  FSelect.Criteria.Equal('age','30');
  FSelect.OrderBy('first_name',oASC);
  FSelect.Limit(0,10);

  Self.AssertEquals(Expect,FSelect.GetClause);
end;

initialization

  RegisterTest(TTestCustomSelect);
end.

