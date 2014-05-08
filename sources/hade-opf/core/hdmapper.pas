unit hdmapper;
{$mode objfpc}{$H+}
{ TODO 2 -oFajar -cMapper : -Create Relation Type one-to-one one-to-many many-to-many }
{ TODO 2 -ofajar -cMapper : improve metadata- CONSTRAINT }
interface

uses
  Classes,
  SysUtils,
  hdmapbase,
  hdbroker,
  hdbase,
  hdobject,
  contnrs;

type
  EHadeMapperException = class(EHadeException);
  ptkInfo = (ptkPK, ptkFK, ptkReadOnly,ptkAutoInc,ptkOID);
  TSetPtkInfo = set of ptkInfo;
  ftType = (ftInteger, ftString, ftBlob, ftReference, ftDateTime, ftSmallInt, ftDouble);
  TRelationType = (rtOneToOne,rtOneToMany,rtManyToMany);

  {$i hademapperintf.inc}

  { THadeMapper }

  THadeMapper = class(THadeBaseObject)
  private
    fDbMapList: THadeDatabaseMapperList;
    FClassMapper: THadeClassMapperList;
    procedure RaiseError(const AMsg: string);
  protected
    procedure CloneMapClass(ASource,ATarget:THadeClassMapper);
  public
    function MapConnection(AConnectionName, ADatabase, AHost, AUsername, APassword: string;
      ABroker: THadeBroker; AIsDefault: boolean = False;AParams:TStringList = nil):THadeDatabaseMapper;
    //using broker AsString no dependency to hdbroker for caller
    function MapConnection(AConnectionName, ADatabase, AHost, AUsername, APassword: string;
      ABrokerString: String; AIsDefault: boolean = False;AParams:TStringList = nil):THadeDatabaseMapper;

    function MapClass(AClass:THadeObjectClass; ATableName: string;
      AExtendClass:THadeObjectClass = nil):THadeClassMapper;

    procedure MapProperties(AClass:THadeObjectClass;APropName, AColName: string;
      APtkInfo: TSetPtkInfo;ADataType: ftType);

    function FindConnection(const AConnectionName:string = ''):THadeDatabaseMapper;
    function FindClassMap(const AClassName:string):THadeClassMapper;
    function FindTableMap(const ATableName:string):THadeClassMapper;

    property Connections: THadeDatabaseMapperList read fDbMapList;
    property ClassLists: THadeClassMapperList read FClassMapper;

    procedure Clear;

    constructor Create;
    destructor Destroy; override;
  end;

implementation
uses
  hdutils;

{ THadeMapper }

procedure THadeMapper.RaiseError(const AMsg: string);
begin
  raise EHadeMapperException.Create(AMsg);
end;

function THadeMapper.MapConnection(AConnectionName, ADatabase, AHost,
  AUsername, APassword: string; ABroker: THadeBroker; AIsDefault: boolean;
  AParams: TStringList): THadeDatabaseMapper;
var
  con: THadeDatabaseMapper;
begin
  con := THadeDatabaseMapper.Create(AConnectionName, AIsDefault);
  con.Database := ADatabase;
  con.Username := AUsername;
  con.Password := APassword;
  con.Host := AHost;
  con.Broker := ABroker;
  if Assigned(AParams) then
    con.Params.Text := AParams.Text;
  fDBMapList.add(AConnectionName, con);

  Result:= FDBMapList.Find(AConnectionName);
end;

procedure THadeMapper.CloneMapClass(ASource,ATarget:THadeClassMapper);
var
  iloop: Integer;
begin
  for iloop:=0 to pred(ASource.Count) do
  begin
    //ATarget.
    ATarget.add(ASource.Items[iloop].PropertyName,
      THadePropertiesMapper.Create(ASource.Items[iloop].PropertyName,
      ASource.Items[iloop].ColumnName,ASource.Items[iloop].ColumnInfo,
      ASource.Items[iloop].DataType),
      False);
  end;
end;

function THadeMapper.MapClass(AClass: THadeObjectClass; ATableName: string;
  AExtendClass: THadeObjectClass): THadeClassMapper;
var
  CSource: THadeClassMapper;
  CTarget: THadeClassMapper;
begin
  CTarget := THadeClassMapper.Create(AClass,ATableName);
  if Assigned(AExtendClass) then
  begin
    CSource := FClassMapper.Find(AExtendClass.ClassName);
    if Assigned(CSource) then
      self.CloneMapClass(CSource,CTarget);
  end;

  FClassMapper.add(AClass.ClassName,CTarget);

  Result:= CTarget;
end;

procedure THadeMapper.MapProperties(AClass: THadeObjectClass; APropName,
  AColName: string; APtkInfo: TSetPtkInfo; ADataType: ftType);
begin
  if not Assigned(FClassMapper.Find(AClass.ClassName)) then
    RaiseError(AClass.ClassName + ' not Registered.');

  FClassMapper.Find(AClass.ClassName).add(
    APropName, THadePropertiesMapper.Create(APropName, AColName, APtkInfo, ADataType));
end;

function THadeMapper.FindConnection(const AConnectionName: string
  ): THadeDatabaseMapper;
begin
  Result:= FDbMapList.Find(AConnectionName);
end;

function THadeMapper.FindClassMap(const AClassName: string): THadeClassMapper;
begin
  Result:= FClassMapper.Find(AClassName);
end;

function THadeMapper.FindTableMap(const ATableName: string): THadeClassMapper;
begin
  Result:= FClassMapper.FindTable(ATableName);
end;

procedure THadeMapper.Clear;
begin
  Self.fDbMapList.Clear;
  Self.FClassMapper.Clear;
end;

function THadeMapper.MapConnection(AConnectionName, ADatabase, AHost,
  AUsername, APassword: string; ABrokerString: String; AIsDefault: boolean;
  AParams: TStringList): THadeDatabaseMapper;
begin
  if hdbroker.StringAsBroker(ABrokerString) = UNKNOWN_BROKER then Self.RaiseError(ABrokerString+' not valid broker.');

  Result:= Self.MapConnection(AConnectionName,ADatabase,AHost,AUsername,APassword,hdbroker.StringAsBroker(ABrokerString),AisDefault,AParams);
end;

constructor THadeMapper.Create;
begin
  fDbMapList := THadeDatabaseMapperList.Create();
  FClassMapper:= THadeClassMapperList.Create();
end;

destructor THadeMapper.Destroy;
begin
  fDbMapList.Free;
  FClassMapper.Free;
  inherited Destroy;
end;

{$i hademapper.inc}

end.
