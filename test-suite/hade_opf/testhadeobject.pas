unit testhadeobject;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry;

type

  { TTestHadeObject }

  TTestHadeObject = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOwner;
    procedure TestOwnerObjectList;
    procedure TestObserved;
    procedure TestObjectList;
    procedure TestSaveJSON;
    procedure TestFromJSON;
    procedure TestLoadJSONList;
    procedure TestSaveJSONList;
  end;

implementation

uses
  hdobject,
  testinit;

procedure TTestHadeObject.TestOwner;
var
  objlist: THadeObjectList;
  obj: THadeObject;
begin
  objlist := THadeObjectList.Create(nil);
  obj := THadeObject.Create(objlist);
  self.AssertEquals(ord(posEmpty),ord(obj.ObjectState));
  try
    objlist.add(obj);
    self.AssertEquals(1,objlist.Count);
  finally
    FreeAndNil(objList);
  end;
end;

procedure TTestHadeObject.TestOwnerObjectList;
var
  objlist: THadeObjectList;
  obj: THadeObject;
begin
  obj := THadeObject.Create(nil);
  self.AssertEquals(ord(posEmpty),ord(obj.ObjectState));
  objlist := THadeObjectList.Create(obj);
  try
    objlist.add(obj);
    self.AssertEquals(1,objlist.Count);
  finally
    FreeAndNil(obj);
  end;
end;

procedure TTestHadeObject.TestObserved;
var
  person: TPerson;
  obs: TTestObserver;
begin
  person := TPerson.Create(nil);
  person.Age := 70;
  obs := TTestObserver.Create;
  try
    person.FPOAttachObserver(obs);
    person.FPONotifyObservers(self, ooChange, nil);
    self.AssertEquals(23, person.Age);
  finally
    FreeAndNil(person);
    FreeAndNil(obs);
  end;
end;

procedure TTestHadeObject.TestObjectList;
var
  objlist: TPersonList;
  person: TPerson;
  idx: PtrUInt;
begin
  objlist := TPersonList.Create(nil);
  try
    //test Add
    person := TPerson.Create(nil);
    idx := objlist.Add(person);
    self.AssertEquals(1, objlist.Count);

    //test delete
    objlist.Delete(idx);//person doesnt get freed because its not child of objlist
    self.AssertEquals(0, objlist.Count);

    //test remove
    objlist.Add(person);//so we can add it again
    objlist.Remove(person);
    self.AssertEquals(0, objlist.Count);
  finally
    FreeAndNil(objlist);
    FreeAndNil(person);
  end;
end;

procedure TTestHadeObject.TestSaveJSON;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  try
    person.personID := 1;
    person.FirstName:= 'Fajar';
    person.LastName:= 'Khairil';
    person.Age:= 23;
    person.Birthday:= sysutils.StrToDate('02-03-1990');

    person.SaveAsJSON('/home/fajar/bin/person.json');
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestHadeObject.TestFromJSON;
var
  person: TPerson;
begin
  person := TPerson.Create(nil);
  try
    person.LoadFromJSON('/home/fajar/bin/person.json');
    self.AssertEquals('Fajar',person.FirstName);
    self.AssertEquals('Khairil',person.LastName);
    self.AssertEquals(23,person.Age);
    self.AssertEquals('1990-03-02',FormatDateTime('yyyy-mm-dd',person.Birthday));
    self.AssertEquals(ord(posCreate),ord(person.ObjectState));
  finally
    FreeAndNil(person);
  end;
end;

procedure TTestHadeObject.TestSaveJSONList;
var
  personlist:array[0..2]of TPerson;
  persons:TPersonList;
  iloop: Integer;
begin
 persons := TPersonList.Create(nil);
 personlist[0] := TPerson.Create(persons);
 personlist[0].FirstName:='Fajar';
 personlist[0].LastName:='Khairil';
 personlist[0].Age:=23;
 personlist[0].Birthday:= StrToDate('02-03-1990');
 persons.add(personlist[0]);
 try
   for iloop:=1 to HIGH(personlist)do
   begin
     personlist[iloop] := TPerson.Create(persons);
     personlist[iloop].FirstName:='Test'+IntToStr(iloop);
     personlist[iloop].LastName:='Silobahutang'+IntToStr(iloop);
     personlist[iloop].Age:= 23+iloop;
     personlist[iloop].Birthday:= StrToDate('02-03-1990');
     persons.add(personlist[iloop]);
   end;
   persons.SaveAsJSON('/home/fajar/bin/person-list.json');
 finally
   FreeAndNil(persons);
 end;
end;

procedure TTestHadeObject.TestLoadJSONList;
var
  personList: TPersonList;
begin
  personList := TPersonList.Create(nil);
  try
    personList.LoadFromJSON('/home/fajar/bin/person-list.json');
    self.AssertEquals('Test1',personList[1].FirstName);
    self.AssertEquals('Silobahutang1',personList[1].LastName);
    self.AssertEquals(24,personList[1].Age);
    self.AssertEquals('1990-03-02',FormatDateTime('yyyy-mm-dd',personList[1].Birthday));
  finally
    FreeAndNil(personList);
  end;
end;

procedure TTestHadeObject.SetUp;
begin
  initMapper;
end;

procedure TTestHadeObject.TearDown;
begin
  clearMapper;
end;

initialization
  RegisterTest(TTestHadeObject);
end.
