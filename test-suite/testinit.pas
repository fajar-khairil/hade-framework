unit testinit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdobject,
  hdimplementorintf;
Const
  //FB_DATABASE_NAME = 'test.fdb';
  SQLITE_DATABASE_NAME = 'person.db';
Type

  TPersonList = class;
  { TCompany }

  TCompany = class(THadeObject)
  private
    FCompID: integer;
    FName: String;
    FPersons: TPersonList;
  published
    property CompanyID:integer read FCompID write FCompID;
    property Name:String read FName write FName;
    property Persons:TPersonList read FPersons;
  end;

  { TPerson }

  TPerson = class(THadeObject)
  private
    FAge: ptrUint;
    FBirthday: TDateTime;
    FCompany: TCompany;
    FFirstName: string;
    Fid: Integer;
    FLastName: string;
    procedure SetAge(AValue: ptrUint);
    procedure SetBirthday(AValue: TDateTime);
    procedure SetFirstName(AValue: string);
    procedure Setid(AValue: Integer);
    procedure SetLastName(AValue: string);
  public
    procedure trigerError;
    destructor Destroy;override;
  published
    property personID:Integer read Fid write Setid;
    property FirstName:string read FFirstName write SetFirstName;
    property LastName:string read FLastName write SetLastName;
    property Age:ptrUint read FAge write SetAge;
    property Birthday:TDateTime read FBirthday write SetBirthday;
    property Company:TCompany read FCompany write FCompany;
  end;

  { TSuperPerson }

  TSuperPerson = class(TPerson)
  private
    FJob: string;
    procedure SetJob(AValue: string);
  published
    property Job:string read FJob write SetJob;
  end;

  { TPersonList }

  TPersonList = class(THadeObjectList)
  private
    function getItem(AIndex: ptrUint): TPerson;
    procedure setItem(AIndex: ptrUint; AValue: TPerson);
  public
    property Items[AIndex: ptrUint]: TPerson read getItem write setItem; default;
    function Add(AObject: TPerson): ptrUint;
    procedure Remove(AObject: TPerson);
  end;

  //class for testing observable class

  { TTestObserver }

  TTestObserver = class(TObject,IFPObserver)
  protected
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  end;

  { TPersonImplementor }

  TPersonImplementor = class(THadeCustomImplementor)
  protected
  public
    procedure Update(AObject: TObject);override;
    procedure Insert(AObject: TObject);override;
    procedure Delete(AObject: TObject);override;
    procedure Select(AObject: TClass; AFetchMode: TFetchMode);override;
  end;

  procedure initMapper;
  procedure clearMapper;

implementation
uses
  hdmapper,
  hdopfmanager,
  hdbroker;

procedure initMapper;
var
  CURR_DIR :string;
  sqClassMapper: THadeClassMapper;
  sqCompanyMapper: THadeClassMapper;
  sqSuperPersonMapper: THadeClassMapper;
begin
  CURR_DIR := ExtractFilePath(ParamStr(0));

  GHadeOPFManager.PersistenceMapper.MapConnection('sqlite',
    CURR_DIR+SQLITE_DATABASE_NAME,
    '127.0.0.1','','','SQLDBSqlite',TRUE);

  //firebird connection
  {GHadeOPFManager.PersistenceMapper.MapConnection('firebird',
    CURR_DIR+FB_DATABASE_NAME,
    '127.0.0.1','SYSDBA','masterkey','SQLDBFirebird');}

  sqClassMapper := GHadeOPFManager.PersistenceMapper.MapClass(TPerson, 'person');
  sqCompanyMapper := GHadeOPFManager.PersistenceMapper.MapClass(TCompany, 'company');

  sqClassMapper.MapProperties('personID','id',[ptkPK,ptkReadOnly,ptkAutoInc],ftInteger);
  sqClassMapper.MapProperties('FirstName','first_name',[],ftString);
  sqClassMapper.MapProperties('LastName','last_name',[],ftString);
  sqClassMapper.MapProperties('Age','age',[],ftInteger);
  sqClassMapper.MapProperties('Birthday','birth',[],ftDateTime);
  sqClassMapper.MapOneToOne('Company','company_id','CompanyID');

  sqSuperPersonMapper := GHadeOPFManager.PersistenceMapper.MapClass(TSuperPerson, 'person',TPerson);
  sqSuperPersonMapper.MapProperties('Job','job',[],ftString);

  sqCompanyMapper.MapProperties('CompanyID','id',[ptkPK,ptkReadOnly],ftInteger);
  sqCompanyMapper.MapProperties('Name','name',[],ftString);
  sqCompanyMapper.MapOneToMany('Persons','company_id','CompanyID');

  //Register List
  TPersonList.ChildClass := TPerson;

  GHadeOPFManager.ImplementorManager.RegisterImplementor('TestImplementor',TSuperPerson,TPersonImplementor,SQLDBFirebird);
end;

procedure clearMapper;
begin
  GHadeOPFManager.PersistenceMapper.Clear;
end;

{ TPersonImplementor }

procedure TPersonImplementor.Update(AObject: TObject);
begin
  WriteLn('Update '+AObject.ClassName+' using implementor : '+Self.ClassName);
end;

procedure TPersonImplementor.Insert(AObject: TObject);
begin
  WriteLn('Insert '+AObject.ClassName+' using implementor : '+Self.ClassName);
end;

procedure TPersonImplementor.Delete(AObject: TObject);
begin
  WriteLn('Delete '+AObject.ClassName+' using implementor : '+Self.ClassName);
end;

procedure TPersonImplementor.Select(AObject: TClass; AFetchMode: TFetchMode);
begin
  FQuery.SQL.Text:= 'Select Upper(person.first_name) as first_name from person';
end;

{ TTestObserver }

procedure TTestObserver.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation = ooChange then
    TPerson(ASender).Age:= 23;
end;

{ TSuperPerson }

procedure TSuperPerson.SetJob(AValue: string);
begin
  if FJob=AValue then Exit;
  FJob:=AValue;
end;

{ TPersonList }

function TPersonList.getItem(AIndex: ptrUint): TPerson;
begin
  Result:= inherited getItem(AIndex) as TPerson;
end;

procedure TPersonList.setItem(AIndex: ptrUint; AValue: TPerson);
begin
  inherited setItem(AIndex,AValue);
end;

function TPersonList.Add(AObject: TPerson): ptrUint;
begin
  Result := Inherited add(AObject);
end;

procedure TPersonList.Remove(AObject: TPerson);
begin
  inherited remove(AObject);
end;

{ TPerson }

procedure TPerson.SetAge(AValue: ptrUint);
begin
  if FAge=AValue then Exit;
  FAge:=AValue;
end;

procedure TPerson.SetBirthday(AValue: TDateTime);
begin
  if FBirthday=AValue then Exit;
  FBirthday:=AValue;
end;

procedure TPerson.SetFirstName(AValue: string);
begin
  if FFirstName=AValue then Exit;
  FFirstName:=AValue;
end;

procedure TPerson.Setid(AValue: Integer);
begin
  if Fid=AValue then Exit;
  Fid:=AValue;
end;

procedure TPerson.SetLastName(AValue: string);
begin
  if FLastName=AValue then Exit;
  FLastName:=AValue;
end;

procedure TPerson.trigerError;
begin
  self.RaiseError('just test!');
end;

destructor TPerson.Destroy;
begin
  inherited Destroy;
end;

end.
