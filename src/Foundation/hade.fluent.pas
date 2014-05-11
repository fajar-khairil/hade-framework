unit hade.Fluent;
{******************************************************************************
 * This File is Part of HadeFramework Project
 *
 * Copyright (C) Fajar Khairil
 * License MPL 1.1
 *
 * Description : Fluent Object Interface
 ******************************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  variants,
  contnrs,
  StrHashMap;
Type

  { EFluent }

  EFluent = class(Exception)End;
  TFluentItem = class;

  //fpc generic interface is still buggy, take clasic approach
  {$INTERFACES CORBA}
  IFluentEnumerator = interface
  ['{3133BED5-AC73-4419-94C3-4691F4528DDC}']
    function GetCurrent: TFluentItem;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TFluentItem read GetCurrent;
  end;


  { TFluent }

  TFluent = class(IFluentEnumerator)
  private
    function getCount: ptrUint;
  protected
    FCursor : ptrUint;
    FKeyList : TStringList;
    FList : TStringHashMap;

    function GetCurrent: TFluentItem;
  public
    procedure clear;
    function First: TFluentItem;
    function Last: TFluentItem;
    function Extract(const ItemKey:shortstring): TFluentItem;
    procedure Remove(const ItemKey:shortstring);

    property List : TStringHashMap read FList;
    property Count : ptrUint read getCount;
    //IEnumerator
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TFluentItem read GetCurrent;

    function Items(const AKeyName:shortstring) : TFluentItem;

    constructor Create;
    Destructor Destroy;override;
  end;

  { TFluentItem }

  TFluentItem = class
  protected
    FValue : Variant;
    function getDate: TDate;
    function getDateTime: TDateTime;
    function getDouble: Double;
    function getInteger: ptrInt;
    function getString: string;
    function getTime: TTime;
    function getUInteger: ptrUint;
    procedure setDate(AValue: TDate);
    procedure setDateTime(AValue: TDateTime);
    procedure setDouble(AValue: Double);
    procedure setInteger(AValue: ptrInt);
    procedure setString(AValue: string);
    procedure setTime(AValue: TTime);
    procedure setUInteger(AValue: ptrUint);

    procedure RaiseError(const AMsg : String);
  public
    property AsString: string read getString write setString;
    property AsInteger : ptrInt read getInteger write setInteger;
    property AsUInteger : ptrUint read getUInteger write setUInteger;
    property AsDouble : Double read getDouble write setDouble;
    property AsDateTime : TDateTime read getDateTime write setDateTime;
    property AsDate : TDate read getDate write setDate;
    property AsTime : TTime read getTime write setTime;
    property Value : Variant read FValue write FValue;

    Constructor Create(const AValue : Variant);
  end;

var
  FluentDefaultFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'yyyy-mm-dd';
    LongDateFormat: 'yyyy" "mmmm" "dd';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

  FluentFormatSetting : TFormatSettings absolute FluentDefaultFormatSettings;

implementation
const
  CEConvertError = 'Unable to convert value to %s.';

{ TFluent }

function TFluent.getCount: ptrUint;
begin
  Result := FList.Count;
end;

function TFluent.GetCurrent: TFluentItem;
begin
  FList.Find(FKeyList[FCursor],Result);
end;

procedure TFluent.clear;
begin
  FList.Iterate(nil,@Iterate_FreeObjects);
  FKeyList.Clear;
  Reset;
end;

function TFluent.First: TFluentItem;
begin
  FCursor := 0;
  FList.find(FKeyList[FCursor],Result);
end;

function TFluent.Last: TFluentItem;
begin
  FCursor := pred(FList.Count);
  FList.find(FKeyList[FCursor],Result);
end;

function TFluent.Extract(const ItemKey: shortstring): TFluentItem;
var
  idx: Integer;
begin
  FList.Find(ItemKey,Result);
  FList.Remove(ItemKey);
  idx := FKeyList.IndexOf(ItemKey);
  FKeyList.Delete( idx );

  if FCursor >= idx then
    FCursor := pred(idx);
end;

procedure TFluent.Remove(const ItemKey: shortstring);
var
  idx: Integer;
begin
  FList.Remove(ItemKey);
  idx := FKeyList.IndexOf(ItemKey);
  FKeyList.Delete( idx );

  if FCursor >= idx then
    FCursor := pred(idx);
end;

function TFluent.MoveNext: Boolean;
begin
  Result := FCursor <> pred(FList.Count);
  if Result then
    inc(FCursor);
end;

procedure TFluent.Reset;
begin
  FCursor := 0;
end;

function TFluent.Items(const AKeyName: shortstring): TFluentItem;
begin
  if FList.Find(AKeyName,Result) then
  begin
    FKeyList.Add(AKeyName);
    exit;
  end else
  begin
    Result := TFluentItem.Create(variants.Null);
    FList.Add(AKeyName,Result);exit;
  end;
end;

constructor TFluent.Create;
begin
  FList:=TStringHashMap.Create(2047,False);
  FKeyList := TStringList.Create;
  FCursor := 0;
  self.Reset;
end;

destructor TFluent.Destroy;
begin
  self.Clear;
  FList.Free;
  FKeyList.Free;
  inherited Destroy;
end;

{ TFluent }

{ TFluentItem }

function TFluentItem.getDate: TDate;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToDate(self.getString,FluentFormatSetting)
  else
    Result := getDouble;
end;

function TFluentItem.getDateTime: TDateTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToDateTime(self.getString,FluentFormatSetting)
  else
    Result := getDouble;
end;

function TFluentItem.getDouble: Double;
begin
  if variants.VarIsNumeric(FValue) then
    Result := Double(FValue)
  else if variants.VarType(FValue) = vardate then
    Result := StrToDateTime(self.getString,FluentFormatSetting)
  else if variants.VarIsStr(FValue) then
    Result := StrToFloatDef(FValue,0)
  else
    RaiseError(Format(CEConvertError,['Double']));
end;

function TFluentItem.getInteger: ptrInt;
begin
  if (variants.VarIsNumeric(FValue)) OR (variants.VarType(FValue) = vardate) then
    Result := ptrInt(FValue)
  else if Variants.VarIsStr(FValue)  then
    if not TryStrToInt(variants.VarToStr(FValue),Result) then
      RaiseError(Format(CEConvertError,['Integer']))
    else
      exit
  else
    RaiseError(Format(CEConvertError,['Integer']));
end;

function TFluentItem.getString: string;
begin
  if variants.VarType(FValue) = vardate then
    Result := DateTimeToStr(FValue,FluentFormatSetting)
  else
    Result := variants.VarToStr(FValue);
end;

function TFluentItem.getTime: TTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToTime(self.getString,FluentFormatSetting)
  else
    Result := getDouble;
end;

function TFluentItem.getUInteger: ptrUint;
begin
  if (variants.VarIsNumeric(FValue)) OR (variants.VarType(FValue) = vardate) then
    Result := ptrUint(FValue)
  else if variants.VarIsStr(FValue) then
    Result := StrToInt(FValue)
  else
    RaiseError(Format(CEConvertError,['Unsigned Integer']));
end;

procedure TFluentItem.setDate(AValue: TDate);
begin
  FValue := AValue;
end;

procedure TFluentItem.setDateTime(AValue: TDateTime);
begin
 FValue := AValue;
end;

procedure TFluentItem.setDouble(AValue: Double);
begin
 FValue := AValue;
end;

procedure TFluentItem.setInteger(AValue: ptrInt);
begin
 FValue := AValue;
end;

procedure TFluentItem.setString(AValue: string);
begin
 FValue := AValue;
end;

procedure TFluentItem.setTime(AValue: TTime);
begin
 FValue := AValue;
end;

procedure TFluentItem.setUInteger(AValue: ptrUint);
begin
 FValue := AValue;
end;

procedure TFluentItem.RaiseError(const AMsg: String);
begin
  raise EFluent.Create(AMsg);
end;

constructor TFluentItem.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

end.

