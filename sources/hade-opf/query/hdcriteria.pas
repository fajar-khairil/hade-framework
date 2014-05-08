unit hdcriteria;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdquerybuilderintf;
Type

  { THadeCriteria }

  THadeCriteria = class(THadeCustomQueryBuilder,IHadeCriteria)
  protected
    FCustCriteria:THadeCustomCriteria;
    FTables: TStringList;
    FHaving:String;

    procedure RaiseError(const Amsg:string);
    procedure init(AObject: TObject);
    function FieldByProp(const AProp:string):string;
  public
    procedure Equal(AExpr1:String;AExpr2:ptrInt);
    procedure Equal(AExpr1:String;AExpr2:Double);
    procedure Equal(AExpr1:String;AExpr2:TDateTime);
    procedure Equal(AExpr1:String;AExpr2:String);

    procedure Greater(AExpr1:String;AExpr2:ptrInt);
    procedure Greater(AExpr1:String;AExpr2:Double);
    procedure Greater(AExpr1:String;AExpr2:TDateTime);
    procedure Greater(AExpr1:String;AExpr2:String);

    procedure Less(AExpr1:String;AExpr2:ptrInt);
    procedure Less(AExpr1:String;AExpr2:Double);
    procedure Less(AExpr1:String;AExpr2:TDateTime);
    procedure Less(AExpr1:String;AExpr2:String);

    procedure GreaterEqual(AExpr1:String;AExpr2:String);
    procedure GreaterEqual(AExpr1:String;AExpr2:ptrInt);
    procedure GreaterEqual(AExpr1:String;AExpr2:Double);
    procedure GreaterEqual(AExpr1:String;AExpr2:TDateTime);

    procedure LessEqual(AExpr1:String;AExpr2:String);
    procedure LessEqual(AExpr1:String;AExpr2:ptrInt);
    procedure LessEqual(AExpr1:String;AExpr2:Double);
    procedure LessEqual(AExpr1:String;AExpr2:TDateTime);

    procedure NotEqual(AExpr1:String;AExpr2:String);
    procedure NotEqual(AExpr1:String;AExpr2:ptrInt);
    procedure NotEqual(AExpr1:String;AExpr2:Double);
    procedure NotEqual(AExpr1:String;AExpr2:TDateTime);

    procedure Like(AExpr1:String;AExpr2:String);
    procedure Like(AExpr1:String;AExpr2:ptrInt);
    procedure Like(AExpr1:String;AExpr2:Double);
    procedure Like(AExpr1:String;AExpr2:TDateTime);

    procedure NotLike(AExpr1:String;AExpr2:String);
    procedure NotLike(AExpr1:String;AExpr2:ptrInt);
    procedure NotLike(AExpr1:String;AExpr2:Double);
    procedure NotLike(AExpr1:String;AExpr2:TDateTime);

    procedure _In(AExpr:String; ACommaValues:string);
    procedure _In(AExpr:String; ACommaValues:array of string);
    procedure _In(AExpr:String; ACommaValues:array of ptrint);
    procedure _In(AExpr:String; ACommaValues:array of double);
    procedure _In(AExpr:String; ACommaValues:array of TDateTime);

    procedure NotIn(AExpr:String; ACommaValues:string);
    procedure NotIn(AExpr:String; ACommaValues:array of string);
    procedure NotIn(AExpr:String; ACommaValues:array of ptrint);
    procedure NotIn(AExpr:String; ACommaValues:array of double);
    procedure NotIn(AExpr:String; ACommaValues:array of TDateTime);

    procedure IsNULL(AExpr:String);
    procedure NotNull(AExpr:String);

    procedure AddNot();
    procedure AddOr();

    procedure Between(AExpr,AExpr1,AExpr2:String);
    procedure Between(AExpr:String;AExpr1,AExpr2:ptrint);
    procedure Between(AExpr:String;AExpr1,AExpr2:double);
    procedure Between(AExpr:String;AExpr1,AExpr2:TDateTime);

    procedure Having(AExpr1:ptrint;AComparison:string;AExpr2:ptrint);
    procedure Having(AExpr1:double;AComparison:string;AExpr2:double);
    procedure Having(AExpr1:TDateTime;AComparison:string;AExpr2:TDateTime);
    procedure Having(AExpr1,AComparison,AExpr2:string);

    //SQL Functions
    function UpperCase(AExpr:String):string;
    function LowerCase(AExpr:String):string;
    function Now:String;
    function CurrentDate:String;
    function CurrentTime:String;
    function CurrentUnixTimestamp:String;

    function DateToString(ADate:TDate):string;
    function TimeToString(ATime:TTime):string;
    function DateTimeToString(ADateTime:TDateTime):string;

    function GetClause: string;override;

    constructor Create(AObject:TObject);
    destructor Destroy;override;
  end;

implementation
uses
  hdopfmanager,
  hdutils,
  hdmapper,
  hdobject;

{ THadeCriteria }

procedure THadeCriteria.RaiseError(const Amsg: string);
begin
  raise EHadeQueryBuilderException.Create(AMsg);
end;

procedure THadeCriteria.Equal(AExpr1: String; AExpr2: String);
begin
  FCustCriteria.Equal(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.Equal(AExpr1: String; AExpr2: ptrInt);
begin
  FCustCriteria.Equal(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.Equal(AExpr1: String; AExpr2: Double);
begin
  FCustCriteria.Equal(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.Equal(AExpr1: String; AExpr2: TDateTime);
begin
  Self.Equal(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.Greater(AExpr1: String; AExpr2: String);
begin
  FCustCriteria.Greater(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.Greater(AExpr1: String; AExpr2: ptrInt);
begin
 FCustCriteria.Greater(FieldByProp(AExpr1),IntTostr(AExpr2));
end;

procedure THadeCriteria.Greater(AExpr1: String; AExpr2: Double);
begin
 FCustCriteria.Greater(FieldByProp(AExpr1),FloatTostr(AExpr2));
end;

procedure THadeCriteria.Greater(AExpr1: String; AExpr2: TDateTime);
begin
 Self.Greater(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.Less(AExpr1: String; AExpr2: String);
begin
 FCustCriteria.Less(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.Less(AExpr1: String; AExpr2: ptrInt);
begin
 FCustCriteria.Less(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.Less(AExpr1: String; AExpr2: Double);
begin
 FCustCriteria.Less(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.Less(AExpr1: String; AExpr2: TDateTime);
begin
 Self.Less(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.GreaterEqual(AExpr1: String; AExpr2: String);
begin
  FCustCriteria.GreaterEqual(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.GreaterEqual(AExpr1: String; AExpr2: ptrInt);
begin
 FCustCriteria.GreaterEqual(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.GreaterEqual(AExpr1: String; AExpr2: Double);
begin
 FCustCriteria.GreaterEqual(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.GreaterEqual(AExpr1: String; AExpr2: TDateTime);
begin
 Self.GreaterEqual(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.LessEqual(AExpr1: String; AExpr2: String);
begin
 FCustCriteria.LessEqual(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.LessEqual(AExpr1: String; AExpr2: ptrInt);
begin
 FCustCriteria.LessEqual(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.LessEqual(AExpr1: String; AExpr2: Double);
begin
 FCustCriteria.LessEqual(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.LessEqual(AExpr1: String; AExpr2: TDateTime);
begin
  Self.LessEqual(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.NotEqual(AExpr1: String; AExpr2: String);
begin
 FCustCriteria.NotEqual(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.NotEqual(AExpr1: String; AExpr2: ptrInt);
begin
 FCustCriteria.NotEqual(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.NotEqual(AExpr1: String; AExpr2: Double);
begin
 FCustCriteria.NotEqual(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.NotEqual(AExpr1: String; AExpr2: TDateTime);
begin
 Self.NotEqual(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.Like(AExpr1: String; AExpr2: String);
begin
  Self.FCustCriteria.Like(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.Like(AExpr1: String; AExpr2: ptrInt);
begin
 Self.FCustCriteria.Like(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.Like(AExpr1: String; AExpr2: Double);
begin
  Self.FCustCriteria.Like(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.Like(AExpr1: String; AExpr2: TDateTime);
begin
 Self.Like(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria.NotLike(AExpr1: String; AExpr2: String);
begin
 FCustCriteria.NotLike(FieldByProp(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.NotLike(AExpr1: String; AExpr2: ptrInt);
begin
 FCustCriteria.NotLike(FieldByProp(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.NotLike(AExpr1: String; AExpr2: Double);
begin
 FCustCriteria.NotLike(FieldByProp(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.NotLike(AExpr1: String; AExpr2: TDateTime);
begin
  Self.NotLike(AExpr1,DateTimeToString(AExpr2));
end;

procedure THadeCriteria._In(AExpr: String; ACommaValues: string);
begin
  FCustCriteria._In(FieldByProp(AExpr),ACommaValues);
end;

procedure THadeCriteria._In(AExpr: String; ACommaValues: array of string);
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + QuotedStr(ACommaValues[iloop]) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self._In(AExpr,tmp);
end;

procedure THadeCriteria._In(AExpr: String; ACommaValues: array of ptrint);
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + IntToStr(ACommaValues[iloop]) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self._In(AExpr,tmp);
end;

procedure THadeCriteria._In(AExpr: String; ACommaValues: array of double);
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + FloatToStr(ACommaValues[iloop]) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self._In(AExpr,tmp);
end;

procedure THadeCriteria._In(AExpr: String;
  ACommaValues: array of TDateTime);
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + QuotedStr(DateTimeToString(ACommaValues[iloop])) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self._In(AExpr,tmp);
end;

procedure THadeCriteria.NotIn(AExpr: String; ACommaValues: string);
begin
  FCustCriteria.NotIn(FieldByProp(AExpr),ACommaValues);
end;

procedure THadeCriteria.NotIn(AExpr: String; ACommaValues: array of string
  );
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + QuotedStr(ACommaValues[iloop]) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self.NotIn(AExpr,tmp);
end;

procedure THadeCriteria.NotIn(AExpr: String; ACommaValues: array of ptrint
  );
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + IntToStr(ACommaValues[iloop]) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self.NotIn(AExpr,tmp);
end;

procedure THadeCriteria.NotIn(AExpr: String; ACommaValues: array of double
  );
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + FloatToStr(ACommaValues[iloop]) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self.NotIn(AExpr,tmp);
end;

procedure THadeCriteria.NotIn(AExpr: String;
  ACommaValues: array of TDateTime);
var
  tmp: String;
  iloop: Integer;
begin
  tmp := '';
  for iloop:=LOW(ACommaValues) to HIGH(ACommaValues)do
  begin
    tmp := tmp + QuotedStr( DateTimeToString(ACommaValues[iloop]) ) +',';
  end;

  tmp := hdutils.deleteLastChar(tmp);
  Self.NotIn(AExpr,tmp);
end;

procedure THadeCriteria.IsNULL(AExpr: String);
begin
  FCustCriteria.IsNULL(FieldByProp(AExpr));
end;

procedure THadeCriteria.NotNull(AExpr: String);
begin
  FCustCriteria.NotNull(FieldByProp(AExpr));
end;

procedure THadeCriteria.AddNot;
begin
  FCustCriteria.AddNot();
end;

procedure THadeCriteria.AddOr;
begin
 FCustCriteria.AddOr();
end;

procedure THadeCriteria.Between(AExpr, AExpr1, AExpr2: String);
begin
  FCustCriteria.Between(FieldByProp(AExpr),QuotedStr(AExpr1),QuotedStr(AExpr2));
end;

procedure THadeCriteria.Between(AExpr: String; AExpr1, AExpr2: ptrint);
begin
  FCustCriteria.Between(FieldByProp(AExpr),IntToStr(AExpr1),IntToStr(AExpr2));
end;

procedure THadeCriteria.Between(AExpr: String; AExpr1, AExpr2: double);
begin
 FCustCriteria.Between(FieldByProp(AExpr),FloatToStr(AExpr1),FloatToStr(AExpr2));
end;

procedure THadeCriteria.Between(AExpr: String; AExpr1, AExpr2: TDateTime);
begin
 Self.Between(AExpr,DateTimeToString(AExpr1),DateTimeToString(AExpr2));
end;

procedure THadeCriteria.Having(AExpr1, AComparison, AExpr2: string);
CONST
  VALID_COMPARISON : array [0..7] of string = ('=','>','<','>=','<=','<>','LIKE','NOT LIKE');
  function CheckComparison:boolean;
  var
    iloop: Integer;
  begin
    Result:= False;
    for iloop:=LOW(VALID_COMPARISON) to HIGH(VALID_COMPARISON)do
      if sysutils.UpperCase(AComparison) = VALID_COMPARISON[iloop] then
      begin
        Result:= True;
        break;
      end;
  end;

begin
  if not CheckComparison then
    Self.RaiseError(AComparison+' invalid comparison.');
  FHaving := 'HAVING '+FieldByProp(AExpr1)+' '+AComparison+' '+AExpr2;
end;

procedure THadeCriteria.Having(AExpr1: ptrint; AComparison: string;
  AExpr2: ptrint);
begin
  Self.Having(IntToStr(AExpr1),AComparison,IntToStr(AExpr2));
end;

procedure THadeCriteria.Having(AExpr1: double; AComparison: string;
  AExpr2: double);
begin
  Self.Having(FloatToStr(AExpr1),AComparison,FloatToStr(AExpr2));
end;

procedure THadeCriteria.Having(AExpr1: TDateTime; AComparison: string;
  AExpr2: TDateTime);
begin
  Self.Having(DateTimeToString(AExpr1),AComparison,DateTimeToString(AExpr2));
end;

function THadeCriteria.UpperCase(AExpr: String): string;
begin
  Result:= FCustCriteria.UpperCase(AExpr);
end;

function THadeCriteria.LowerCase(AExpr: String): string;
begin
  Result:= FCustCriteria.LowerCase(AExpr);
end;

function THadeCriteria.Now: String;
begin
  Result:= FCustCriteria.Now;
end;

function THadeCriteria.CurrentDate: String;
begin
  Result:= FCustCriteria.CurrentDate;
end;

function THadeCriteria.CurrentTime: String;
begin
  Result:= FCustCriteria.CurrentTime;
end;

function THadeCriteria.CurrentUnixTimestamp: String;
begin
  Result:= FCustCriteria.CurrentTime;
end;

function THadeCriteria.DateToString(ADate: TDate): string;
begin
 Result:= FCustCriteria.DateToString(ADate);
end;

function THadeCriteria.TimeToString(ATime: TTime): string;
begin
  Result:= FCustCriteria.TimeToString(ATime);
end;

function THadeCriteria.DateTimeToString(ADateTime: TDateTime): string;
begin
  Result:= FCustCriteria.DateTimeToString(ADateTime);
end;

function THadeCriteria.GetClause: string;
var
  tmp: String;
begin
  tmp:= FCustCriteria.GetClause+' '+FHaving;
  Result:= TrimRight(tmp);
end;

procedure THadeCriteria.init(AObject: TObject);
var
  pClassName: String;
  AClassMap: THadeClassMapper;
  iloop: Integer;
begin
  pClassName := '';
  if AObject is THadeObjectList then
    pClassName := THadeObjectList(AObject).ChildClass.ClassName
  else if AObject is THadeObject then
    pClassName:= AObject.ClassName
  else begin
    RaiseError('Unsuported Object Type.');
  end;

  AClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap(pClassName);

  if Assigned(AClassMap) then
  begin
    FTables.Add(pClassName);
    for iloop:=0 to pred(AClassMap.RelationList.Count) do
      FTables.Add( GHadeOPFManager.PersistenceMapper.FindTableMap(AClassMap.RelationList.NameOfIndex(iloop))._ClassName );
  end else
    RaiseError(pClassName+' not registered.');
end;

function THadeCriteria.FieldByProp(const AProp: string): string;
  function SplitClassName(var ATableName:string):string;
  var
    idx:integer;
  begin
    Result:= '';
    idx:= system.Pos('.',AProp);
    if idx = 0 then
      Exit
    else begin
      ATableName := copy(AProp,0,idx-1);
      Result := copy(AProp,idx + 1,length(AProp) - idx);
    end;
  end;

var
  iloop: Integer;
  ClassMap: THadeClassMapper;
  prop: THadePropertiesMapper;
  Found:boolean;
  ATable: string;
  FTmp: String;
begin
  Result:= '';
  Found:= False;

  FTmp := SplitClassName(ATable);

  if FTmp <> EmptyStr then
  begin
    ClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap(ATable);
    if Assigned( ClassMap )then
    begin
      prop := ClassMap.Find(FTmp);
      if Assigned(Prop) then
      begin
        Result:= Prop.ColumnName;
        exit;
      end;
    end;
  end;

  for iloop:=0 to pred(FTables.Count)do
  begin
    ClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap(FTables[iloop]);
    prop := ClassMap.Find(AProp);
    if not Assigned(prop) then
      continue
    else
    begin
      Result := prop.ColumnName;
      Found:=True;
      break;
    end;
  end;

  if not Found then
    RaiseError('Cannot find '+AProp+' on mapper.');
end;

constructor THadeCriteria.Create(AObject: TObject);
begin
  FCustCriteria:= THadeCustomCriteria.Create;
  FTables := TStringList.Create;
  init(AObject);
end;

destructor THadeCriteria.Destroy;
begin
  FCustCriteria.Free;
  FTables.Free;
  inherited Destroy;
end;

end.

