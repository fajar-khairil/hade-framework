unit hade.Fluent;

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
  public
    property AsString: string read getString write setString;
    property AsInteger : ptrInt read getInteger write setInteger;
    property AsUInteger : ptrUint read getUInteger write setUInteger;
    property AsDouble : Double read getDouble write setDouble;
    property AsDateTime : TDateTime read getDateTime write setDateTime;
    property AsDate : TDate read getDate write setDate;
    property AsTime : TTime read getTime write setTime;
  end;

implementation

{ TFluent }

function TFluent.getDate: TDate;
begin
  if variants.VarIsFloat(FValue) then
    Result := variants.VarToDateTime(FValue)
  else
    raise EFluent.Create('Unable to Convert value to Double');
end;

function TFluent.getDateTime: TDateTime;
begin
  if variants.VarIsFloat(FValue) then
    Result := self.getDate
  else
    raise EFluent.Create('Unable to Convert value to Double');
end;

function TFluent.getDouble: Double;
begin
  if variants.VarIsFloat(FValue) then
    Result := Double(FValue)
  else
    raise EFluent.Create('Unable to Convert value to Double');
end;

function TFluent.getInteger: ptrInt;
begin
  if variants.VarIsType(FValue) then
    Result := ptrInt(FValue)
  else
    raise EFluent.Create('Unable to Convert value to Double');
end;

function TFluent.getString: string;
begin
  if variants.VarIsFloat(FValue) then
    Result := Double(FValie)
  else
    raise EFluent.Create('Unable to Convert value to Double');
end;

function TFluent.getTime: TTime;
begin

end;

function TFluent.getUInteger: ptrUint;
begin

end;

procedure TFluent.setDate(AValue: TDate);
begin

end;

procedure TFluent.setDateTime(AValue: TDateTime);
begin

end;

procedure TFluent.setDouble(AValue: Double);
begin

end;

procedure TFluent.setInteger(AValue: ptrInt);
begin

end;

procedure TFluent.setString(AValue: string);
begin

end;

procedure TFluent.setTime(AValue: TTime);
begin

end;

procedure TFluent.setUInteger(AValue: ptrUint);
begin

end;

end.

