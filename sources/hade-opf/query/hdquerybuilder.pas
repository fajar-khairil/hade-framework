unit hdquerybuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdquerybuilderintf,
  hdbroker;

CONST
  oASC =  TOrderType.oASC;
  oDESC = TOrderType.oDESC;
  jtLeft = TJoinType.jtLeft;
  jtRight = TJoinType.jtRight;
  jtInner = TJoinType.jtInner;
  jtOuter = TJoinType.jtOuter;

Type
  { THadeQueryBuilderSelect }

  THadeQueryBuilderSelect = class(THadeCustomQueryBuilder,IHadeSQLSelect)
  protected
    FAggregate:THadeCustomQueryAggregate;
    FCriteria: THadeCustomCriteria;
    FSelect,Ftable,FHaving,FGroupBy,FOrderBy,FLimit,FJoin:string;
    function _on(AExpr1,Aexpr2:string):string;
    function GetCriteria: THadeCustomCriteria;
  public
    procedure Select(ACommaFields:String);
    procedure Join(ATable,AExpr1,Aexpr2:string;AJoinType:TJoinType = jtInner);
    procedure GroupBy(AExpr:string);
    procedure OrderBy(AExpr:string;AOrderType:TOrderType = oASC);
    procedure Limit(AOffset,ALimit:ptrUint);virtual;abstract;
    procedure From(ATables:string);
    procedure Having(AExpr1,AComparison,AExpr2:string);

    property Criteria : THadeCustomCriteria read GetCriteria;

    //Aggregate Functions
    function Sum(AExpr:String):String;
    function Avg(AExpr:String):String;
    function Count(AExpr:String = '*'):String;
    function Max(AExpr:String):String;
    function Min(AExpr:String):String;

    function GetClause:string;override;

    constructor Create();
    destructor Destroy;override;
  end;

  { THadeQueryBuilderInsert }

  THadeQueryBuilderInsert = class(THadeCustomQueryBuilder,IHadeSQLInsert)
  protected
    FFields,FTable,FValues:string;
  public
    procedure Insert(ATableName,ACommaFields:string);
    procedure Values(ACommaValues : String);
  end;

  { THadeQueryBuilderUpdate }

  THadeQueryBuilderUpdate = class(THadeCustomQueryBuilder,IHadeSQLUpdate)
  protected
    FCriteria:THadeCustomCriteria;
    FTable,FFields,FValues:String;
    function GetCriteria: THadeCustomCriteria;
    function CreateSetValues:string;
  public
    procedure Update(Const ATableName,AFields:string);
    procedure Values(Const ACommaValues:string);
    property Criteria : THadeCustomCriteria read GetCriteria;

    function GetClause:string;override;

    destructor Destroy;override;
  end;

  { THadeQueryBuilderDelete }

  THadeQueryBuilderDelete = Class(THadeCustomQueryBuilder,IHadeSQLDelete)
  protected
    FTable:String;
    FCriteria:THadeCustomCriteria;
    function GetCriteria: THadeCustomCriteria;
  public
    procedure Delete(ATableName:string);
    property Criteria : THadeCustomCriteria read GetCriteria;

    function GetClause:string;override;

    destructor Destroy;override;
  end;

  { THadeFirebirdBuilderInsert }

  THadeFirebirdBuilderInsert = class(THadeQueryBuilderInsert)
  public
    function GetClause:string;override;
  end;

  { THadeMySQLBuilderInsert }

  THadeMySQLBuilderInsert = class(THadeQueryBuilderInsert)
  public
    function GetClause:string;override;
  end;

  { THadeMySQLBuilderSelect }

  THadeMySQLBuilderSelect = Class(THadeQueryBuilderSelect)
  public
    procedure Limit(AOffset,ALimit:ptrUint);override;
  end;

  { THadeFirebirdBuilderSelect }

  THadeFirebirdBuilderSelect = Class(THadeQueryBuilderSelect)
  public
    procedure Limit(AOffset,ALimit:ptrUint);override;
  end;

  { THadeQueryBuilderFactory }

  THadeQueryBuilderFactory = Class
  public
    class function GetSelectBuilder(ABroker:THadeBroker):THadeQueryBuilderSelect;
    class function GetInsertBuilder(ABroker:THadeBroker):THadeQueryBuilderInsert;
    class function GetUpdateBuilder(ABroker:THadeBroker):THadeQueryBuilderUpdate;
    class function GetDeleteBuilder(ABroker:THadeBroker):THadeQueryBuilderDelete;
  end;

implementation
uses
  hdutils;

{ THadeQueryBuilderDelete }

function THadeQueryBuilderDelete.GetCriteria: THadeCustomCriteria;
begin
  if not Assigned(FCriteria) then
    FCriteria:= THadeCustomCriteria.Create;
  Result:= FCriteria;
end;

procedure THadeQueryBuilderDelete.Delete(ATableName: string);
begin
  FTable:= ATableName;
end;

function THadeQueryBuilderDelete.GetClause: string;
var
  WHERE: String;
begin
  WHERE := '';
  if Assigned(FCriteria) then
    WHERE := ' '+FCriteria.GetClause;
  Result:= 'DELETE FROM '+FTable+WHERE;
end;

destructor THadeQueryBuilderDelete.Destroy;
begin
  if Assigned(FCriteria) then
    FCriteria.Free;
  inherited Destroy;
end;

{ THadeQueryBuilderUpdate }

function THadeQueryBuilderUpdate.GetCriteria: THadeCustomCriteria;
begin
  if not Assigned(FCriteria) then
    FCriteria:= THadeCustomCriteria.Create;
  Result:= FCriteria;
end;

function THadeQueryBuilderUpdate.CreateSetValues: string;
var
  AFieldList: TStringList;
  AValueList: TStringList;
  iloop: Integer;
begin
  Result:='';
  AFieldList:= TStringList.Create;
  AValueList:= TStringList.Create;
  try
    AFieldList.CommaText:= FFields;
    AValueList.CommaText:= FValues;
    if AFieldList.Count <> AValueList.Count then
      RaiseError('Fields and Values length doesn`t match.');

    for iloop:=0 to pred(AFieldList.count) do
    begin
      Result:= Result+AFieldList[iloop]+' = '+AValueList[iloop]+',';
    end;

    Result:= hdutils.DeleteLastChar(Result);
  finally
    AFieldList.Free;
    AValueList.Free;
  end;
end;

procedure THadeQueryBuilderUpdate.Update(const ATableName, AFields: string);
begin
  FTable:= ATableName;
  FFields:= AFields;
end;

procedure THadeQueryBuilderUpdate.Values(const ACommaValues: string);
begin
  FValues:= ACommaValues;
end;

function THadeQueryBuilderUpdate.GetClause: string;
var
  WHERE: String;
begin
  WHERE := '';
  if Assigned(FCriteria) then
    WHERE := ' '+FCriteria.GetClause;
  Result:= 'UPDATE '+FTable+' SET '+CreateSetValues+WHERE;
end;

destructor THadeQueryBuilderUpdate.Destroy;
begin
  if Assigned(FCriteria) then
    FCriteria.Free;
  inherited Destroy;
end;

{ THadeMySQLBuilderInsert }

function THadeMySQLBuilderInsert.GetClause: string;
begin
  Result:= 'INSERT INTO '+FTable+' ('+FFields+') VALUES('+FValues+')';
end;

{ THadeFirebirdBuilderInsert }

function THadeFirebirdBuilderInsert.GetClause: string;
begin
  Result:= 'INSERT INTO '+FTable+' ('+FFields+') VALUES('+FValues+') RETURNING id';
end;

{ THadeQueryBuilderInsert }

procedure THadeQueryBuilderInsert.Insert(ATableName, ACommaFields: string);
begin
  FTable:=ATableName;
  FFields:= ACommaFields;
end;

procedure THadeQueryBuilderInsert.Values(ACommaValues: String);
begin
  FValues:= ACommaValues;
end;

{ THadeQueryBuilderFactory }

class function THadeQueryBuilderFactory.GetSelectBuilder(ABroker: THadeBroker
  ): THadeQueryBuilderSelect;
begin
  Case ABroker of
    SQLDBMySQL,SQLDBSqlite: Result:= THadeMySQLBuilderSelect.Create();
    SQLDBFirebird: Result:= THadeFirebirdBuilderSelect.Create();
    else
      Raise EHadeQueryBuilderException.Create('Broker not yet implemented.');
  end;
end;

class function THadeQueryBuilderFactory.GetInsertBuilder(ABroker: THadeBroker
  ): THadeQueryBuilderInsert;
begin
  Case ABroker of
    SQLDBMySQL,SQLDBSqlite: Result:= THadeMySQLBuilderInsert.Create();
    SQLDBFirebird: Result:= THadeFirebirdBuilderInsert.Create();
    else
      Raise EHadeQueryBuilderException.Create('Broker not yet implemented.');
  end;
end;

class function THadeQueryBuilderFactory.GetUpdateBuilder(ABroker: THadeBroker
  ): THadeQueryBuilderUpdate;
begin
  Result:= THadeQueryBuilderUpdate.Create();
end;

class function THadeQueryBuilderFactory.GetDeleteBuilder(ABroker: THadeBroker
  ): THadeQueryBuilderDelete;
begin
  Result:= THadeQueryBuilderDelete.Create();
end;

{ THadeFirebirdBuilderSelect }

procedure THadeFirebirdBuilderSelect.Limit(AOffset,ALimit: ptrUint);
begin
  if AOffset < 1 then AOffset := 1;
  FLimit:= 'ROWS '+IntToStr(AOffset)+' TO '+IntToStr(ALimit);
end;

{ THadeMySQLBuilderSelect }

procedure THadeMySQLBuilderSelect.Limit(AOffset,ALimit: ptrUint);
begin
  if AOffset < 0 then AOffset := 0;
  FLimit:= 'LIMIT '+IntToStr(AOffset)+','+IntToStr(ALimit);
end;

{ THadeQueryBuilderSelect }

function THadeQueryBuilderSelect.Sum(AExpr: String): String;
begin
  Result:= FAggregate.Sum(AExpr);
end;

function THadeQueryBuilderSelect.Avg(AExpr: String): String;
begin
  Result:= FAggregate.Avg(AExpr);
end;

function THadeQueryBuilderSelect.Count(AExpr: String): String;
begin
  Result:= FAggregate.Count(AExpr);
end;

function THadeQueryBuilderSelect.Max(AExpr: String): String;
begin
  Result:= FAggregate.Max(AExpr);
end;

function THadeQueryBuilderSelect.Min(AExpr: String): String;
begin
  Result:= FAggregate.Min(AExpr);
end;

function THadeQueryBuilderSelect.GetClause: string;
var
  tmp:string;
  WHERE: String;
begin
  Result :='SELECT '+FSelect+' FROM '+FTable;
  WHERE := '';
  if Assigned(FCriteria) then
    WHERE := FCriteria.GetClause;

  tmp :=' '+FJoin+TrimLeft(WHERE+' ')+
       TrimLeft(FGroupBy+' ')+TrimLeft(FHaving+' ')+TrimLeft(FOrderBy+' ')+FLimit;

  Result:= Result+TrimRight(tmp);
end;

function THadeQueryBuilderSelect.GetCriteria: THadeCustomCriteria;
begin
  if not Assigned(FCriteria) then
    FCriteria:= THadeCustomCriteria.Create;

  Result:= FCriteria;
end;

function THadeQueryBuilderSelect._on(AExpr1, Aexpr2: string):string;
begin
  Result:= AExpr1+' = '+AExpr2;
end;

procedure THadeQueryBuilderSelect.Select(ACommaFields: String);
begin
  FSelect:= ACommaFields;
end;

procedure THadeQueryBuilderSelect.Join(ATable, AExpr1, Aexpr2: string;
  AJoinType: TJoinType);
begin
  FJoin:= FJoin+joinTypeToString(AJoinType)+' '+ATable+' ON('+Self._on(AExpr1,AExpr2)+') ';
end;

procedure THadeQueryBuilderSelect.GroupBy(AExpr:string);
begin
  FGroupBy:= 'GROUP BY '+AExpr;
end;

procedure THadeQueryBuilderSelect.OrderBy(AExpr: string; AOrderType: TOrderType);
begin
  FOrderBy:= 'ORDER BY '+AExpr+' '+OrderTypeToString(AOrderType);
end;

procedure THadeQueryBuilderSelect.From(ATables: string);
begin
  FTable:= ATables;
end;

procedure THadeQueryBuilderSelect.Having(AExpr1, AComparison, AExpr2: string);
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
  FHaving := 'HAVING '+AExpr1+' '+AComparison+' '+AExpr2;
end;

constructor THadeQueryBuilderSelect.Create;
begin
  inherited Create;
  FAggregate:= THadeCustomQueryAggregate.Create;
end;

destructor THadeQueryBuilderSelect.Destroy;
begin
  FAggregate.Free;
  if Assigned(FCriteria) then
    FCriteria.Free;
  inherited Destroy;
end;

end.
