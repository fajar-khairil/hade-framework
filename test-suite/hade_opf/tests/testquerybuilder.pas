unit testquerybuilder;

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

  { TTestQueryBuilder }

  TTestQueryBuilder = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    //using default connection sqldb firebird
    procedure TestSelect;
    procedure TestSelectBetWeen;
    procedure TestDelete;
    procedure TestInsert;
    procedure TestUpdate;
    //using sqlite
    procedure TestSQLiteSelect;
    procedure TestSQLiteSelectBetWeen;
    procedure TestSQLiteDelete;
    procedure TestSQLiteInsert;
    procedure TestSQLiteUpdate;
  end;

implementation

uses
  testinit,
  //strutils,
  hdmapper,
  dateutils;

procedure TTestQueryBuilder.TestSelect;
var
  VSelect: THadeQueryBuilderSelect;
  expect: String;
begin
  VSelect := THadeQueryBuilderSelect.Create('firebird','first_name,last_name,age','person');
  expect := 'SELECT person.first_name,person.last_name,person.age '+
            'FROM person LEFT JOIN contact on(person.contact_id = contact.id) '+
            'GROUP BY first_name ORDER BY first_name ASC ROWS 1 TO 10';
  try
    VSelect.Join('contact','person.contact_id','contact.id',jtLeft);
    VSelect.Limit(10,1);
    VSelect.orderBy('first_name',oASC);
    VSelect.GroupBy('first_name');
    self.AssertEquals(uppercase(expect),uppercase(VSelect.Clause));
  finally
    FreeAndNil(VSelect);
  end;
end;

procedure TTestQueryBuilder.TestSelectBetWeen;
var
  VSelect: THadeQueryBuilderSelect;
  expect: String;
begin
  VSelect := THadeQueryBuilderSelect.Create('firebird','birth','person');
  expect := 'SELECT person.birth '+
            'FROM person WHERE birth BETWEEN 1990-03-02 AND 1991-03-02 ROWS 1 TO 10';
  try
    VSelect.between('birth',StrToDate('02-03-1990'),StrToDate('02-03-1991'));
    VSelect.Limit(10,1);
    self.AssertEquals(uppercase(expect),uppercase(VSelect.Clause));
  finally
    FreeAndNil(VSelect);
  end;
end;

procedure TTestQueryBuilder.TestDelete;
var
  vDelete: THadeQueryBuilderDelete;
  expect: String;
begin
  vDelete := THadeQueryBuilderDelete.create('firebird','person');
  expect:= 'DELETE FROM PERSON WHERE age = 23';
  try
    vDelete.where('age','=',23);
    self.AssertEquals(uppercase(expect),uppercase(vDelete.clause));
  finally
      FreeAndNil(vDelete);
  end;
end;

procedure TTestQueryBuilder.TestInsert;
var
  vInsert: THadeQueryBuilderInsert;
  expect: String;
begin
  vInsert := THadeQueryBuilderInsert.Create('firebird','person');
  expect:= 'INSERT INTO PERSON(first_name,last_name) VALUES(''Fajar'',''Khairil'') RETURNING id';
  try
     vInsert.Values('first_name,last_name',['Fajar','Khairil']);
    self.AssertEquals(uppercase(expect),uppercase(vInsert.clause));
  finally
      FreeAndNil(vInsert);
  end;
end;

procedure TTestQueryBuilder.TestUpdate;
var
  vUpdate: THadeQueryBuilderUpdate;
  expect: String;
begin
  vUpdate := THadeQueryBuilderUpdate.Create('firebird','person');
  expect:= 'UPDATE PERSON SET age = 23 WHERE birth = 1990-03-02';
  try
    vUpdate.Values('age',[23]);
    vUpdate.where('birth','=',StrToDateTime('02-03-1990'));
    self.AssertEquals(uppercase(expect),uppercase(vUpdate.clause));
  finally
      FreeAndNil(vUpdate);
  end;
end;

procedure TTestQueryBuilder.TestSQLiteSelect;
var
  VSelect: THadeQueryBuilderSelect;
  expect: String;
begin
  VSelect := THadeQueryBuilderSelect.Create('sqlite','first_name,last_name,age','person');
  expect := 'SELECT person.first_name,person.last_name,person.age '+
            'FROM person LEFT JOIN contact on(person.contact_id = contact.id) '+
            'GROUP BY first_name ORDER BY first_name ASC LIMIT 10,0';
  try
    VSelect.Join('contact','person.contact_id','contact.id',jtLeft);
    VSelect.Limit(10,1);
    VSelect.orderBy('first_name',oASC);
    VSelect.GroupBy('first_name');
    self.AssertEquals(uppercase(expect),uppercase(VSelect.Clause));
  finally
    FreeAndNil(VSelect);
  end;
end;

procedure TTestQueryBuilder.TestSQLiteSelectBetWeen;
var
  VSelect: THadeQueryBuilderSelect;
  expect: String;
begin
  VSelect := THadeQueryBuilderSelect.Create('sqlite','birth','person');
  expect := 'SELECT person.birth '+
            'FROM person WHERE birth BETWEEN 1990-03-02 AND 1991-03-02 LIMIT 10,0';
  try
    VSelect.between('birth',StrToDate('02-03-1990'),StrToDate('02-03-1991'));
    VSelect.Limit(10,1);
    self.AssertEquals(uppercase(expect),uppercase(VSelect.Clause));
  finally
    FreeAndNil(VSelect);
  end;
end;

procedure TTestQueryBuilder.TestSQLiteDelete;
var
  vDelete: THadeQueryBuilderDelete;
  expect: String;
begin
  vDelete := THadeQueryBuilderDelete.Create('sqlite','person');
  expect:= 'DELETE FROM PERSON WHERE age = 23';
  try
    vDelete.where('age','=',23);
    self.AssertEquals(uppercase(expect),uppercase(vDelete.clause));
  finally
      FreeAndNil(vDelete);
  end;
end;

procedure TTestQueryBuilder.TestSQLiteInsert;
var
  vInsert: THadeQueryBuilderInsert;
  expect: String;
begin
  vInsert := THadeQueryBuilderInsert.Create('sqlite','person');
  expect:= 'INSERT INTO PERSON(first_name,last_name) VALUES(''Fajar'',''Khairil'')';
  try
     vInsert.Values('first_name,last_name',['Fajar','Khairil']);
    self.AssertEquals(uppercase(expect),uppercase(vInsert.clause));
  finally
      FreeAndNil(vInsert);
  end;
end;

procedure TTestQueryBuilder.TestSQLiteUpdate;
var
  vUpdate: THadeQueryBuilderUpdate;
  expect: String;
begin
  vUpdate := THadeQueryBuilderUpdate.Create('sqlite','person');
  expect:= 'UPDATE PERSON SET age = 23 WHERE birth = 1990-03-02';
  try
    vUpdate.Values('age',[23]);
    vUpdate.where('birth','=',StrToDateTime('02-03-1990'));
    self.AssertEquals(uppercase(expect),uppercase(vUpdate.clause));
  finally
      FreeAndNil(vUpdate);
  end;
end;

procedure TTestQueryBuilder.SetUp;
begin
  initMapper;
end;

procedure TTestQueryBuilder.TearDown;
begin
  clearMapper;
end;

initialization

  RegisterTest(TTestQueryBuilder);
end.
