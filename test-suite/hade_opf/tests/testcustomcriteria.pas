unit testcustomcriteria;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  hdquerybuilderintf;

type

  { TTestCustomCriteria }

  TTestCustomCriteria= class(TTestCase)
  protected
    FCriteria:THadeCustomCriteria;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleCriteria;
    procedure TestInCriteria;
    procedure TestDateTimeCriteria;
    procedure TestDateCriteria;
    procedure TestTimeCriteria;
    procedure TestBetween;

    procedure TestUpperCase;
    procedure TestLowerCase;
    procedure TestNow;
    procedure TestCurrentDate;
    procedure TestCurrentTime;
    procedure TestCurrentUnixTimestamp;
  end;

implementation
{ TTestCustomCriteria }

procedure TTestCustomCriteria.TestSimpleCriteria;
var
  AExpect: String;
begin
  AExpect:= uppercase('WHERE first_name = fajar AND last_name = Khairil OR AGE = 30');
  FCriteria.Equal('first_name','Fajar');
  FCriteria.Equal('last_name','Khairil');
  FCriteria.AddOr();
  FCriteria.Equal('AGE','30');
  Self.AssertEquals(AExpect,Uppercase(FCriteria.GetClause));
end;

procedure TTestCustomCriteria.TestInCriteria;
var
  AExpect: String;
begin
  AExpect:= 'WHERE AGE IN(30,23,21)';
  FCriteria._In('AGE','30,23,21');
  Self.AssertEquals(AExpect,Uppercase(FCriteria.GetClause));
end;

procedure TTestCustomCriteria.TestDateTimeCriteria;
var
  ABirth: TDateTime;
  AExpect: String;
begin
  //DateTimeToString test
  ABirth:= Sysutils.StrToDateTime('02-03-1990 04:39:40');
  AExpect:= 'WHERE BIRTH = 1990-03-02 04:39:40';
  FCriteria.Equal('BIRTH',FCriteria.DateTimeToString(ABirth));
  Self.AssertEquals(AExpect,Uppercase(FCriteria.GetClause));
end;

procedure TTestCustomCriteria.TestDateCriteria;
var
  ABirth: TDateTime;
  AExpect: String;
begin
  //DateToString test
  ABirth:= Sysutils.StrToDate('02-03-1990');
  AExpect:= 'WHERE BIRTH = 1990-03-02';
  FCriteria.Equal('BIRTH',FCriteria.DateToString(ABirth));
  Self.AssertEquals(AExpect,Uppercase(FCriteria.GetClause));
end;

procedure TTestCustomCriteria.TestTimeCriteria;
var
  ABirth: TDateTime;
  AExpect: String;
begin
  //TimeToString test
  ABirth:= Sysutils.StrToTime('04:39:40');
  AExpect:= 'WHERE BIRTH = 04:39:40';
  FCriteria.Equal('BIRTH',FCriteria.TimeToString(ABirth));
  Self.AssertEquals(AExpect,Uppercase(FCriteria.GetClause));
end;

procedure TTestCustomCriteria.TestBetween;
var
  ABirth: TDateTime;
  AExpect: String;
  ABirth2: TDateTime;
begin
  //TimeToString test
  ABirth:= Sysutils.StrToDate('02-03-1990');
  ABirth2:= Sysutils.StrToDate('02-03-1995');
  AExpect:= 'WHERE BIRTH BETWEEN 1990-03-02 AND 1995-03-02';
  FCriteria.Between('BIRTH',FCriteria.DateToString(ABirth),FCriteria.DateToString(ABirth2));
  Self.AssertEquals(AExpect,Uppercase(FCriteria.GetClause));
end;

procedure TTestCustomCriteria.TestUpperCase;
var
  AExpect: String;
begin
  FCriteria.Equal('first_name',FCriteria.UpperCase('fajar'));
  AExpect := 'WHERE first_name = UPPER(fajar)';
  Self.AssertEquals(AExpect,FCriteria.GetClause);
end;

procedure TTestCustomCriteria.TestLowerCase;
var
  AExpect: String;
begin
  FCriteria.Equal('first_name',FCriteria.LowerCase('fajar'));
  AExpect := 'WHERE first_name = LOWER(fajar)';
  Self.AssertEquals(AExpect,FCriteria.GetClause);
end;

procedure TTestCustomCriteria.TestNow;
var
  Aexpect: String;
begin
  Aexpect:='WHERE BIRTH = '+FCriteria.Now;
  FCriteria.Equal('BIRTH',FCriteria.Now);
  Self.AssertEquals(AExpect,UpperCase(FCriteria.GetClause));
  WriteLn('FCriteria.Now = '+FCriteria.Now);
end;

procedure TTestCustomCriteria.TestCurrentDate;
var
  Aexpect: String;
begin
  Aexpect:='WHERE BIRTH = '+FCriteria.CurrentDate;
  FCriteria.Equal('BIRTH',FCriteria.CurrentDate);
  Self.AssertEquals(AExpect,UpperCase(FCriteria.GetClause));
  WriteLn('FCriteria.CurrentDate = '+FCriteria.CurrentDate);
end;

procedure TTestCustomCriteria.TestCurrentTime;
var
  Aexpect: String;
begin
  Aexpect:='WHERE BIRTH = '+FCriteria.CurrentTime;
  FCriteria.Equal('BIRTH',FCriteria.CurrentTime);
  Self.AssertEquals(AExpect,UpperCase(FCriteria.GetClause));
  WriteLn('FCriteria.CurrentTime = '+FCriteria.CurrentTime);
end;

procedure TTestCustomCriteria.TestCurrentUnixTimestamp;
var
  Aexpect: String;
begin
  Aexpect:='WHERE BIRTH = '+FCriteria.CurrentUnixTimestamp;
  FCriteria.Equal('BIRTH',FCriteria.CurrentUnixTimestamp);
  Self.AssertEquals(AExpect,UpperCase(FCriteria.GetClause));
  WriteLn('FCriteria.CurrentUnixTimestamp = '+FCriteria.CurrentUnixTimestamp);
end;

procedure TTestCustomCriteria.SetUp;
begin
  FCriteria:= THadeCustomCriteria.Create();
end;

procedure TTestCustomCriteria.TearDown;
begin
  FCriteria.Free;
end;

initialization

  RegisterTest(TTestCustomCriteria);
end.

