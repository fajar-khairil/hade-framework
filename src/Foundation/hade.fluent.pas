unit hade.Fluent;
{******************************************************************************
 * This File is Part of HadeFramework Project
 *
 * Copyright (C) Fajar Khairil
 * License MPL 1.1
 *
 * Description : Fluent Object Interface
 * trying to mimic the ideas of Laravel(php framework) Fluent interface
 * to pascal world.
 ******************************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  variants,
  StrHashMap;
Type
  {$i fluentintf.inc}

  TFluentItem = class;
  TFluent = specialize TCustomFluent<TFluentItem>;

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
    Constructor Create;overload;
  end;

implementation
uses
  hade.Utils;
const
  CEConvertError = 'Unable to convert value to %s.';

{$i fluentimpl.inc}

{ TFluentItem }

function TFluentItem.getDate: TDate;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToDate(self.getString,HadeFormatSetting)
  else
    Result := getDouble;
end;

function TFluentItem.getDateTime: TDateTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToDateTime(self.getString,HadeFormatSetting)
  else
    Result := getDouble;
end;

function TFluentItem.getDouble: Double;
begin
  if variants.VarIsNumeric(FValue) then
    Result := Double(FValue)
  else if variants.VarType(FValue) = vardate then
    Result := StrToDateTime(self.getString,HadeFormatSetting)
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
    Result := DateTimeToStr(FValue,HadeFormatSetting)
  else
    Result := variants.VarToStr(FValue);
end;

function TFluentItem.getTime: TTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := StrToTime(self.getString,HadeFormatSetting)
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

constructor TFluentItem.Create;
begin
  FValue := variants.Null;
end;

end.

