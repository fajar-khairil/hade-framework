unit testcriteria;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  hdcriteria,
  testinit;

type

  { TTestCriteria }

  TTestCriteria= class(TTestCase)
  protected
    criteria :THadeCriteria;
    persons: TPersonList;
    person: TPerson;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimple;
  end;

implementation

procedure TTestCriteria.TestSimple;
var
  expect: String;
begin
  criteria.Equal('FirstName',person.FirstName);
  criteria.Equal('Age',person.Age);
  criteria.Equal('Birthday',person.Birthday);

  expect:= 'WHERE first_name = '+QuotedStr('Fajar')+
           ' AND age = 23 AND birth = '+QuotedStr('1990-03-02');

  Self.AssertEquals(UpperCase(expect),UpperCase(criteria.GetClause));
end;

procedure TTestCriteria.SetUp;
begin
 initmapper;
 persons:= TPersonList.Create(nil);
 criteria := THadeCriteria.Create(persons);
 person:= TPerson.Create(persons);
 persons.Add(person);

 person.FirstName:='Fajar';
 person.LastName:= 'Khairil';
 person.Age:= 23;
 person.Birthday:= StrToDate('02-03-1990');
end;

procedure TTestCriteria.TearDown;
begin
 criteria.Free;
 persons.Free;
 clearmapper;
end;

initialization

  RegisterTest(TTestCriteria);
end.

