unit hdquerybuilderintf;

{$mode objfpc}{$H+}
interface

uses
  Classes,
  SysUtils,
  hdbase,
  StringBuilderUnit;

type
  TOrderType = (oASC, oDESC);
  TJOinType = (jtLeft,jtRight,jtInner,jtOuter);

  EHadeQueryBuilderException = EHadeException;

  {$INTERFACES CORBA}

  { IHadeSQLDDL }

  IHadeSQLDDL = interface
    ['{4EDB1F05-718E-4F50-8D22-612E399DE5C8}']
  end;

  { IHadeQueryBuilder }

  IHadeQueryBuilder = interface
  ['{43584E50-0453-42D0-ADF0-5DB0FC5543E0}']
    function GetClause: string;
  end;

  IHadeCriteriaAggregate = interface
  ['{FA790D9A-9191-4C56-9C65-D13E0ECBCC4C}']
    function Sum(AExpr:String):String;
    function Avg(AExpr:String):String;
    function Count(AExpr:String = '*'):String;
    function Max(AExpr:String):String;
    function Min(AExpr:String):String;
  end;

  { IHadeCriteria }

  IHadeCriteria = interface(IHadeQueryBuilder)
  ['{1AEBC5EA-AB51-418C-9A66-532C1C451545}']
    procedure Equal(AExpr1:String;AExpr2:String);
    procedure Greater(AExpr1:String;AExpr2:String);
    procedure Less(AExpr1:String;AExpr2:String);
    procedure GreaterEqual(AExpr1:String;AExpr2:String);
    procedure LessEqual(AExpr1:String;AExpr2:String);
    procedure NotEqual(AExpr1:String;AExpr2:String);

    procedure Like(AExpr1:String;AExpr2:String);
    procedure NotLike(AExpr1:String;AExpr2:String);
    procedure _In(AExpr:String; ACommaValues:string);
    procedure NotIn(AExpr:String; ACommaValues:string);
    procedure IsNULL(AExpr:String);
    procedure NotNull(AExpr:String);
    procedure AddNot();
    procedure AddOr();

    procedure Between(AExpr,AExpr1,AExpr2:String);

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
  end;

  THadeCustomCriteria = class;
  { IHadeSQLSelect }

  IHadeSQLSelect = interface(IHadeQueryBuilder)
  ['{DB4C82DC-76CB-4ABD-878F-E80A2520A7C7}']
    function GetCriteria: THadeCustomCriteria;
    procedure Select(ACommaFields:String);
    procedure Join(ATable,AExpr1,Aexpr2:string;AJoinType:TJoinType = jtInner);
    procedure GroupBy(AExpr:string);
    procedure OrderBy(AExpr:string;AOrderType:TOrderType = oASC);
    procedure Limit(AOffset,ALimit:ptrUint);
    procedure From(ATables:string);
    procedure Having(AExpr1,AComparison,AExpr2:string);

    property Criteria : THadeCustomCriteria read GetCriteria;
  end;

  { IHadeSQLInsert }

  IHadeSQLInsert = interface(IHadeQueryBuilder)
  ['{EC440C9B-97F4-4E24-A8FA-B003BAD6C1A5}']
    procedure Insert(ATableName,ACommaFields:string);
    procedure Values(ACommaValues : String);
  end;

  { IHadeSQLUpdate }

  IHadeSQLUpdate = interface(IHadeQueryBuilder)
  ['{BC414B54-DE2B-4490-B8B2-DDF268CF98B0}']
    function GetCriteria: THadeCustomCriteria;
    procedure Update(Const ATableName,AFields:string);
    procedure Values(Const ACommaValues:string);
    property Criteria : THadeCustomCriteria read GetCriteria;
  end;

  { IHadeSQLDelete }

  IHadeSQLDelete = interface(IHadeQueryBuilder)
  ['{826149D2-C61A-4029-9705-2E6C1464A58D}']
    procedure Delete(ATableName:string);
    function GetCriteria: THadeCustomCriteria;
    property Criteria : THadeCustomCriteria read GetCriteria;
  end;

  { THadeCustomQueryBuilder }

  THadeCustomQueryBuilder = Class(THadeBaseObject)
  protected
    procedure RaiseError(Const AMsg:string);
  public
    function GetClause:string;virtual;abstract;
  end;

  { THadeCustomQueryAggregate }

  THadeCustomQueryAggregate = class(THadeBaseObject,IHadeCriteriaAggregate)
  public
    function Sum(AExpr:String):String;
    function Avg(AExpr:String):String;
    function Count(AExpr:String = '*'):String;
    function Max(AExpr:String):String;
    function Min(AExpr:String):String;
  end;

  { THadeCustomCriteria }

  THadeCustomCriteria = class(THadeCustomQueryBuilder,IHadeCriteria)
  protected
    FBuffer : TStringBuilder;
    FOnORState:boolean;
    FHaving:string;
    function AddExprSeperator:string;
    procedure AddExpresion(AExpression:string);
  public
    procedure Equal(AExpr1:String;AExpr2:String);
    procedure Greater(AExpr1:String;AExpr2:String);
    procedure Less(AExpr1:String;AExpr2:String);
    procedure GreaterEqual(AExpr1:String;AExpr2:String);
    procedure LessEqual(AExpr1:String;AExpr2:String);
    procedure NotEqual(AExpr1:String;AExpr2:String);

    procedure Like(AExpr1:String;AExpr2:String);
    procedure NotLike(AExpr1:String;AExpr2:String);
    procedure _In(AExpr:String;ACommaValues:string);
    procedure NotIn(AExpr:String; ACommaValues:string);
    procedure IsNULL(AExpr:String);
    procedure NotNull(AExpr:String);
    procedure AddNot();
    procedure AddOr();

    procedure Between(AExpr,AExpr1,AExpr2:String);

    function GetClause:string;override;

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

    constructor Create;
    destructor Destroy;override;
  end;

  function orderTypeToString(AOrderType: TOrderType): string;
  function joinTypeToString(AJoinType: TJoinType): string;

implementation
uses
  hdutils,
  dateutils;

function orderTypeToString(AOrderType: TOrderType): string;
begin
  case (AOrderType) of
    oASC: Result := 'ASC';
    oDESC: Result := 'DESC';
  end;
end;

function joinTypeToString(AJoinType: TJoinType): string;
begin
  case (AJoinType) of
    jtLeft: Result := 'LEFT JOIN';
    jtRight: Result := 'RIGHT JOIN';
    jtInner: Result := 'INNER JOIN';
    jtOuter: Result := 'OUTER JOIN';
  end;
end;

{ THadeCustomQueryAggregate }

function THadeCustomQueryAggregate.Sum(AExpr: String): String;
begin
  Result:= 'SUM('+AExpr+')';
end;

function THadeCustomQueryAggregate.Avg(AExpr: String): String;
begin
  Result:= 'AVG('+AExpr+')';
end;

function THadeCustomQueryAggregate.Count(AExpr: String): String;
begin
    Result:= 'COUNT('+AExpr+')';
end;

function THadeCustomQueryAggregate.Max(AExpr: String): String;
begin
  Result:= 'MAX('+AExpr+')';
end;

function THadeCustomQueryAggregate.Min(AExpr: String): String;
begin
  Result:= 'MIN('+AExpr+')';
end;

{ THadeCustomQueryBuilder }

procedure THadeCustomQueryBuilder.RaiseError(const AMsg: string);
begin
  Raise EHadeQueryBuilderException.Create(AMsg);
end;

{ THadeCustomCriteria }

function THadeCustomCriteria.AddExprSeperator: string;
begin
  Result:='';
  if Self.FBuffer.TotalLength = 0 then exit;

  if FOnOrState then
  begin
    Result:= ' OR ';
    FOnOrState:= False;
  end
  else
    Result := ' AND ';
end;

procedure THadeCustomCriteria.AddExpresion(AExpression:string);
begin
  FBuffer.Add( Self.AddExprSeperator+AExpression);
end;

procedure THadeCustomCriteria.Equal(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' = '+AExpr2);
end;

procedure THadeCustomCriteria.Greater(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' > '+AExpr2);
end;

procedure THadeCustomCriteria.Less(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' < '+AExpr2);
end;

procedure THadeCustomCriteria.GreaterEqual(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' >= '+AExpr2);
end;

procedure THadeCustomCriteria.LessEqual(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' <= '+AExpr2);
end;

procedure THadeCustomCriteria.NotEqual(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' <> '+AExpr2);
end;

procedure THadeCustomCriteria.Like(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' LIKE '+AExpr2);
end;

procedure THadeCustomCriteria.NotLike(AExpr1: String; AExpr2: String);
begin
  Self.AddExpresion(AExpr1+' NOT LIKE '+AExpr2);
end;

procedure THadeCustomCriteria._In(AExpr: String; ACommaValues:string);
begin;
  Self.AddExpresion(AExpr+' IN('+ACommaValues+')');
end;

procedure THadeCustomCriteria.NotIn(AExpr: String; ACommaValues: string);
begin
  Self.AddExpresion(AExpr+' NOT IN('+ACommaValues+')');
end;

procedure THadeCustomCriteria.IsNULL(AExpr: String);
begin
  Self.AddExpresion(AExpr+' ISNULL');
end;

procedure THadeCustomCriteria.NotNull(AExpr: String);
begin
  Self.AddExpresion(AExpr+' IS NOT NULL');
end;

procedure THadeCustomCriteria.AddNot;
begin
  Self.AddExpresion('NOT');
end;

procedure THadeCustomCriteria.AddOr;
begin
  Self.FOnORState:= True;
end;

procedure THadeCustomCriteria.Between(AExpr, AExpr1, AExpr2: String);
begin
  Self.AddExpresion(AExpr+' BETWEEN '+AExpr1+' AND '+AExpr2);
end;

function THadeCustomCriteria.GetClause: string;
var
  tmp: String;
begin
  tmp:= 'WHERE '+TrimRight(FBuffer.ToString)+' '+FHaving;
  Result:= TrimRight(tmp);
end;

function THadeCustomCriteria.UpperCase(AExpr: String): string;
begin
  Result:= 'UPPER('+AExpr+')';
end;

function THadeCustomCriteria.LowerCase(AExpr: String): string;
begin
  Result:= 'LOWER('+AExpr+')';
end;

function THadeCustomCriteria.Now: String;
begin
  Result:= Self.DateTimeToString(Sysutils.Now);
end;

function THadeCustomCriteria.CurrentDate: String;
begin
  Result:= Self.DateToString(Sysutils.Now);
end;

function THadeCustomCriteria.CurrentTime: String;
begin
  Result:= Self.TimeToString(Sysutils.Now);
end;

function THadeCustomCriteria.CurrentUnixTimestamp: String;
begin
  Result:= IntToStr( DateUtils.DateTimeToUnix(Sysutils.Now) );
end;

function THadeCustomCriteria.DateToString(ADate: TDate): string;
begin
 Result:= Sysutils.DateToStr(ADate,HadeDBFormatSettings);
end;

function THadeCustomCriteria.TimeToString(ATime: TTime): string;
begin
  Result:= Sysutils.TimeToStr(ATime,HadeDBFormatSettings);
end;

function THadeCustomCriteria.DateTimeToString(ADateTime: TDateTime): string;
begin
  Result:= Sysutils.DateTimeToStr(ADateTime,HadeDBFormatSettings);
end;

constructor THadeCustomCriteria.Create;
begin
  FBuffer := TStringBuilder.Create;
  FOnORState:= False;
end;

destructor THadeCustomCriteria.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

end.
