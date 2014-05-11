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
  variants;
Type

  { EFluent }

  EFluent = class(Exception);
  { TFluent }

  TFluent = class
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

function TFluent.getDate: TDate;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToDate(self.getString,FluentFormatSetting)
  else
    Result := getDouble;
end;

function TFluent.getDateTime: TDateTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToDateTime(self.getString,FluentFormatSetting)
  else
    Result := getDouble;
end;

function TFluent.getDouble: Double;
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

function TFluent.getInteger: ptrInt;
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

function TFluent.getString: string;
begin
  if variants.VarType(FValue) = vardate then
    Result := DateTimeToStr(FValue,FluentFormatSetting)
  else
    Result := variants.VarToStr(FValue);
end;

function TFluent.getTime: TTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToTime(self.getString,FluentFormatSetting)
  else
    Result := getDouble;
end;

function TFluent.getUInteger: ptrUint;
begin
  if (variants.VarIsNumeric(FValue)) OR (variants.VarType(FValue) = vardate) then
    Result := ptrUint(FValue)
  else if variants.VarIsStr(FValue) then
    Result := StrToInt(FValue)
  else
    RaiseError(Format(CEConvertError,['Unsigned Integer']));
end;

procedure TFluent.setDate(AValue: TDate);
begin
  FValue := AValue;
end;

procedure TFluent.setDateTime(AValue: TDateTime);
begin
 FValue := AValue;
end;

procedure TFluent.setDouble(AValue: Double);
begin
 FValue := AValue;
end;

procedure TFluent.setInteger(AValue: ptrInt);
begin
 FValue := AValue;
end;

procedure TFluent.setString(AValue: string);
begin
 FValue := AValue;
end;

procedure TFluent.setTime(AValue: TTime);
begin
 FValue := AValue;
end;

procedure TFluent.setUInteger(AValue: ptrUint);
begin
 FValue := AValue;
end;

procedure TFluent.RaiseError(const AMsg: String);
begin
  raise EFluent.Create(AMsg);
end;

constructor TFluent.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

end.

