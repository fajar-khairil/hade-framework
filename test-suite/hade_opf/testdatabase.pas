unit testdatabase;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  testinit,
  hddatabase;

type

  { TTestDatabase }

  TTestDatabase = class(TTestCase)
  private
    database: THadeDatabase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRead;
    procedure TestReadWithRelation;
    procedure TestObjectListRead;
    procedure TestUpplyUpdate;
    //procedure TestTransaction;
    procedure TestSave;
    procedure TestUpdate;
    procedure TestDelete;
    procedure TestUseCustomImplementor;
  end;

implementation

uses
  hdobject,
  hdcriteria;

procedure TTestDatabase.TestRead;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  try
    person.personID := 1;
    database.Read(person);
    self.AssertEquals('Fajar', person.FirstName);
    self.AssertEquals('Khairil', person.LastName);
    self.AssertEquals(23, person.Age);
    self.AssertEquals(Ord(posPK), Ord(person.Company.ObjectState));
    self.AssertEquals(1, person.Company.CompanyID);
    self.AssertEquals('02-03-1990', FormatDateTime('dd-mm-yyyy', person.Birthday));
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestDatabase.TestReadWithRelation;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  try
    person.personID := 1;
    database.Read(person, fcJoin);
    self.AssertEquals('Fajar', person.FirstName);
    self.AssertEquals('Khairil', person.LastName);
    self.AssertEquals(23, person.Age);
    self.AssertEquals(Ord(posPK), Ord(person.Company.ObjectState));
    self.AssertEquals(1, person.Company.CompanyID);
    Self.AssertEquals('Hade-Software', person.Company.Name);
    self.AssertEquals('02-03-1990', FormatDateTime('dd-mm-yyyy', person.Birthday));
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestDatabase.TestObjectListRead;
var
  persons: TPersonList;
begin
  persons := TPersonList.Create(nil);
  try
    persons.Criteria.Less('Age',30);
    database.Read(persons);

    Self.AssertEquals(5,persons.Count);
    Self.AssertEquals('Fajar',persons.Items[0].FirstName);
  finally
    persons.Free;
  end;
end;

procedure TTestDatabase.TestUpplyUpdate;
var
  persons: TPersonList;
  iloop: Integer;
begin
  persons := TPersonList.Create(nil);
  try
    persons.Criteria.Less('Age',30);
    database.Read(persons);

    for iloop := 0 to pred(persons.Count) do
    begin
      persons.Items[iloop].FirstName:= 'Another';
      persons.Items[iloop].LastName:= 'Test';
      persons.Items[iloop].MarkModified;
    end;

    //delete last object from database
    persons.Items[ pred(persons.Count) ].MarkDelete;

    database.ApplyUpdate(persons);

    Self.AssertEquals('Another',persons.Items[0].FirstName);
    Self.AssertEquals('Test',persons.Items[0].LastName);
    self.AssertEquals(9, database.RecordsCount('person'));
  finally
    persons.Free;
  end;
end;

{procedure TTestDatabase.TestTransaction;
begin
  //self.Fail('not yet implemented');
end;}

procedure TTestDatabase.TestSave;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  person.FirstName := 'Jhon';
  person.LastName := 'Doe';
  person.Age := 32;
  person.Birthday := strToDate('02-03-1990');
  try
    database.Save(person);
    Self.AssertEquals(11, person.personID);
    self.AssertEquals(Ord(posClean), Ord(person.ObjectState));
    self.AssertEquals(11, database.RecordsCount('person'));
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestDatabase.TestUpdate;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  try
    person.personID := 1;
    database.Read(person);
    self.AssertEquals(Ord(posClean), Ord(person.ObjectState));
    self.AssertEquals('Fajar', person.FirstName);

    person.FirstName := 'Tokay';
    person.MarkModified;
    database.Save(person);
    self.AssertEquals(Ord(posClean), Ord(person.ObjectState));
    self.AssertEquals('Tokay', person.FirstName);
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestDatabase.TestDelete;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  try
    person.personID := 1;
    database.Read(person);
    self.AssertEquals(Ord(posClean), Ord(person.ObjectState));
    self.AssertEquals('Fajar', person.FirstName);

    person.MarkDelete;
    database.Save(person);
    self.AssertEquals(Ord(posDeleted), Ord(person.ObjectState));
    self.AssertEquals(9, database.RecordsCount('person'));
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestDatabase.TestUseCustomImplementor;
var
  person: TSuperPerson;
begin
  person:= TSuperPerson.Create(nil);
  person.personID:= 1;
  try
    database.Read(person);
    Self.AssertEquals('FAJAR',person.FirstName);
  finally
    person.Free;
  end;
end;

procedure TTestDatabase.SetUp;
begin
  initMapper();
  database := THadeDatabase.Create();
  database.StartTransaction();
  try
    database.ExecuteScripts( ExtractFilePath(ParamStr(0)) + 'person.sql' );
    self.AssertEquals(10, database.RecordsCount('person'));
  except
    on E:Exception do
    begin
      database.Rollback();
      Self.Fail(E.Message);
    end;
  end;
end;

procedure TTestDatabase.TearDown;
begin
  database.Commit();
  database.Free;
  clearMapper();
end;

initialization

  RegisterTest(TTestDatabase);
end.
