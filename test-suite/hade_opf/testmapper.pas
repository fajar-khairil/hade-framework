unit testmapper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  hdmapper;

type

  { TTestMapper }

  TTestMapper = class(TTestCase)
  private

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetPK;
    procedure TestGetDefaultConnection;
    procedure TestGetWriteableProp;
    procedure TestGetDefaultProp;
    procedure TestGetPropertiesList;
    procedure TestGetProperty;
    procedure TestExtendMapClass;
    procedure TestGetColumnList;
    procedure TestFindByColumn;
    procedure TestClassList;
    procedure TestOneToOneRelation;
    procedure TestOneToManyRelation;
  end;

implementation

uses
  testinit,
  hdutils,
  hdopfmanager;

procedure TTestMapper.SetUp;
begin
  initMapper;
end;

procedure TTestMapper.TearDown;
begin
  ClearMapper;
end;

procedure TTestMapper.TestGetPK;
var
  vResult: THadePropertiesMapper;
begin
  vResult := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').getPK;
  self.AssertEquals('personID', vResult.PropertyName);
end;

procedure TTestMapper.TestGetDefaultConnection;
var
  vResult: string;
begin
  vResult := GHadeOPFManager.PersistenceMapper.Connections.Find().ConnectionName;
  self.AssertEquals('sqlite', vResult);
end;

procedure TTestMapper.TestGetWriteableProp;
var
  vResult: TStringList;
  vClassMap: THadeClassMapper;
  vExpected: TStringList;
begin
  vExpected := TStringList.Create;
  vExpected.Add('FirstName');
  vExpected.Add('LastName');
  vExpected.Add('Birthday');
  vExpected.Add('Age');
  vExpected.Add('Company');
  try
    vClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').getWriteableProperties;
    vResult := vClassMap.getPropertiesList;

    self.AssertTrue(hdutils.compareStringList(vExpected, vResult));
  finally
    FreeAndNil(vResult);
    FreeAndNil(vExpected);
    FreeAndNil(vClassMap);
  end;
end;

procedure TTestMapper.TestGetDefaultProp;
var
  vResult: THadePropertiesMapper;
begin
  vResult := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').getPK;
  self.AssertEquals('id', vResult.ColumnName);
end;

procedure TTestMapper.TestGetPropertiesList;
var
  vResult: TStringList;
  vExpected: TStringList;
begin
  vExpected := TStringList.Create;
  vExpected.Add('personID');
  vExpected.Add('FirstName');
  vExpected.Add('LastName');
  vExpected.Add('Birthday');
  vExpected.Add('Age');
  vExpected.Add('Company');
  vResult := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').getPropertiesList;
  try
    self.AssertTrue(hdutils.compareStringList(vExpected, vResult));
  finally
    FreeAndNil(vResult);
    FreeAndNil(vExpected);
  end;
end;

procedure TTestMapper.TestGetProperty;
var
  vResult: THadePropertiesMapper;
begin
  vResult := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').Properties['LastName'];

  self.AssertEquals('last_name', vResult.ColumnName);
end;

procedure TTestMapper.TestExtendMapClass;
var
  ListExpected: TStringList;
  ListResult: TStringList;
begin
  self.AssertEquals(ExtractFilePath(ParamStr(0))+SQLITE_DATABASE_NAME, GHadeOPFManager.PersistenceMapper.Connections.Find().Database);
  self.AssertEquals('person', GHadeOPFManager.PersistenceMapper.FindClassMap(
    'TSuperPerson').Table);

  ListExpected := TStringList.Create;
  ListExpected.Add('personID');
  ListExpected.Add('FirstName');
  ListExpected.Add('LastName');
  ListExpected.Add('Birthday');
  ListExpected.Add('Age');
  ListExpected.Add('Company');
  ListExpected.Add('Job');
  ListResult := GHadeOPFManager.PersistenceMapper.FindClassMap('TSuperPerson').getPropertiesList;
  self.AssertTrue(hdutils.compareStringList(ListExpected, ListResult, True));
end;

procedure TTestMapper.TestGetColumnList;
var
  ListExpected: TStringList;
  ListResult: TStringList;
begin
  ListExpected := TStringList.Create;
  ListExpected.Add('id');
  ListExpected.Add('first_name');
  ListExpected.Add('last_name');
  ListExpected.Add('birth');
  ListExpected.Add('age');
  ListExpected.Add('company_id');
  ListResult := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').getColumnLists;
  self.AssertTrue(hdutils.compareStringList(ListExpected, ListResult, True));
end;

procedure TTestMapper.TestFindByColumn;
var
  vResult: THadePropertiesMapper;
begin
  vResult:= GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson').FindByColumn('first_name');
  self.AssertEquals('FirstName',vResult.PropertyName);
end;

procedure TTestMapper.TestClassList;
begin
  self.AssertEquals('TPerson',TPersonList.ChildClass.ClassName );
end;

procedure TTestMapper.TestOneToOneRelation;
var
  ClassMap: THadeClassMapper;
begin
  ClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap('TPerson');
  self.AssertEquals(uppercase('CompanyID'),uppercase(ClassMap.RelationList.Find('Company').RelationProperty));
  self.AssertEquals(uppercase('Company'),uppercase(ClassMap.RelationList.Find('Company').PropertyName));
end;

procedure TTestMapper.TestOneToManyRelation;
var
  ClassMap: THadeClassMapper;
begin
  ClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap('TCompany');
  self.AssertEquals(uppercase('company_id'),uppercase(ClassMap.RelationList.Find('Persons').RelationProperty));
  self.AssertEquals(uppercase('CompanyID'),uppercase(ClassMap.RelationList.Find('Persons').PropertyName));
end;

initialization

  RegisterTest(TTestMapper);
end.
