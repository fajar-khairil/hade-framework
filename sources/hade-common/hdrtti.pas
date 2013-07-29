{******************************Version: MPL 1.1*********************************
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with
* the License. You may obtain a copy of the License at
* http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* Initial Developer : Fajar Khairil
*
* Portions created by the Initial Developer are Copyright (C) 2010 - 2011
* the Initial Developer. All Rights Reserved.
*
********************************************************************************
* This File is Part of HdFramework OPF Library
* Copyright (C) 2010 - 2011 HdFramework Developer Team
*
* Unit : hade.opf.RTTI
* Description : hdframework RTTI routine.
* Author: - Fajar Khairil
*******************************************************************************}
unit hdrtti;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  typinfo;

//Get List Name of published property from object/class
function GetPropInfoList(const AObject: TObject): TStringList; overload;
function GetPropInfoList(const AClass: TClass): TStringList; overload;

function IsIntegerProp(AObject: TObject; APropName: string): boolean;
function IsStringProp(AObject: TObject; APropName: string): boolean;

function IsReadWriteProp(AObject: TObject; APropName: string): boolean;
function IsReadOnlyProp(AObject: TObject; APropName: string): boolean;

procedure setStringProp(AObj: TObject; APropName, AValue: string);
function getStringProp(Aobj: TObject; APropName: string): string;

procedure setIntegerProp(Aobj: TObject; APropName: string; AValue: integer);
function getIntegerProp(Aobj: TObject; APropName: string): integer;

procedure setOrdinalProp(Aobj: TObject; APropName: string; AValue: integer);
function getOrdinalProp(Aobj: TObject; APropName: string): integer;

procedure setEnumbyString(AObj: TObject; APropName: string; AValue: string);

procedure setFloatProp(AObj: TObject; APropName: string; AValue: double);
function getFloatProp(AObj: TObject; APropName: string): double;

procedure setVarProp(AObj: TObject; APropName: string; AValue: variant);
function getVarProp(AObj: TObject; APropName: string): variant;

procedure sethdObjectProp(AObj: TObject; APropName: string; AValue: TObject);
function getHdObjectProp(AObj: TObject; APropName: string): TObject;

procedure sethdInterfaceProp(AObj: TObject; APropName: string; AValue: IInterface);
function getHdInterfaceProp(AObj: TObject; APropName: string): IInterface;

//procedure setHdObjectClassProp(AObject: TObject; ApropName: string; AValue: TClass);
function getHdObjectClassProp(AObj: TClass; APropName: string): TClass;
function getHdObjectClassProp(AObj: TObject; APropName: string): TClass;
const
  tkSimple = [tkInteger, tkFloat, tkSString, tkLString, tkAString,
    tkWString, tkUString, tkUChar, tkChar, tkInt64];

implementation

function GetPropInfoList(const AObject: TObject): TStringList;
var
  Count, Loop: integer;
  FList: PPropList;
begin
  Result := TStringList.Create;
  Count := GetPropList(AObject.ClassInfo, FList);
  try
    Result.BeginUpdate;
    for Loop := 0 to Pred(Count) do
      Result.Add(FList^[Loop]^.Name);
    Result.EndUpdate;
  finally
    FreeMem(FList,Count*sizeof(pointer));
  end;
end;

function GetPropInfoList(const AClass: TClass): TStringList;
var
  Count, Loop: integer;
  FList: PPropList;
begin
  Result := TStringList.Create;
  Count := GetPropList(PTypeInfo(AClass.ClassInfo), FList);
  try
    Result.BeginUpdate;
    for Loop := 0 to Pred(Count) do
      Result.Add(FList^[Loop]^.Name);
    Result.EndUpdate;
  finally
    FreeMem(FList,Count*sizeof(pointer));
  end;
end;

function IsIntegerProp(AObject: TObject; APropName: string): boolean;
var
  lPropType: TTypeKind;
begin
  lPropType := PropType(AObject, APropName);
  if lPropType in [tkInteger, tkInt64] then
    Result := True
  else
    Result := False;
end;

function IsStringProp(AObject: TObject; APropName: string): boolean;
var
  lPropType: TTypeKind;
begin
  lPropType := PropType(AObject, APropName);
  if lPropType in [tkChar, tkSString, tkLString, tkAString, tkWString,
    tkUString, tkUChar] then
    Result := True
  else
    Result := False;
end;

function IsReadWriteProp(AObject: TObject; APropName: string): boolean;
var
  lPropInfo: PPropInfo;
begin
  Result := False;
  lPropInfo := typinfo.GetPropInfo(AObject, APropName);
  Result := (lPropInfo^.GetProc <> nil) and (lPropInfo^.SetProc <> nil);
end;

function IsReadOnlyProp(AObject: TObject; APropName: string): boolean;
var
  lPropInfo: PPropInfo;
begin
  Result := False;
  lPropInfo := typinfo.GetPropInfo(AObject, APropName);
  Result := (lPropInfo^.SetProc = nil);
end;

procedure setStringProp(AObj: TObject; APropName, AValue: string);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PAnsiString(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typinfo.SetStrProp(AObj, APropName, AValue);
end;

function getStringProp(Aobj: TObject; APropName: string): string;
begin
  Result := typinfo.GetStrProp(AObj, APropName);
end;

procedure setIntegerProp(Aobj: TObject; APropName: string; AValue: integer);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PPtrUint(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    SetInt64Prop(AObj, APropName, AValue);
end;

function getIntegerProp(Aobj: TObject; APropName: string): integer;
begin
  Result := typinfo.GetInt64Prop(AObj, APropName);
end;

procedure setOrdinalProp(Aobj: TObject; APropName: string; AValue: integer);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PPtrUint(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typinfo.SetOrdProp(AObj, APropName, AValue);
end;

function getOrdinalProp(Aobj: TObject; APropName: string): integer;
begin
  Result := typinfo.GetOrdProp(AObj, APropName);
end;

procedure setEnumbyString(AObj: TObject; APropName: string; AValue: string);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PAnsiString(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typinfo.SetEnumProp(AObj, APropName, AValue);
end;

procedure setFloatProp(AObj: TObject; APropName: string; AValue: double);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PDouble(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typinfo.SetFloatProp(AObj, APropName, AValue);
end;

function getFloatProp(AObj: TObject; APropName: string): double;
begin
  Result := typinfo.GetFloatProp(AObj, APropName);
end;

procedure setVarProp(AObj: TObject; APropName: string; AValue: variant);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PVariant(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typInfo.SetPropValue(AObj, APropName, AValue);
end;

function getVarProp(AObj: TObject; APropName: string): variant;
begin
  Result := typinfo.GetPropValue(AObj, ApropName);
end;

procedure sethdObjectProp(AObj: TObject; APropName: string; AValue: TObject);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PPointer(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typInfo.SetObjectProp(AObj, APropName, AValue);
end;

function getHdObjectProp(AObj: TObject; APropName: string): TObject;
begin
  Result := typInfo.GetObjectProp(AObj, ApropName);
end;

procedure sethdInterfaceProp(AObj: TObject; APropName: string; AValue: IInterface);
begin
  if IsReadOnlyProp(Aobj, APropName) then
  begin
    PInterface(ptrUint(Aobj) + (ptrUint(GetPropInfo(Aobj, APropName)^.GetProc) and
      $00FFFFFF))^ := AValue;
  end
  else
    typInfo.SetInterfaceProp(AObj, APropName, AValue);
end;

function getHdInterfaceProp(AObj: TObject; APropName: string): IInterface;
begin
  Result := typinfo.GetInterfaceProp(AObj, APropName);
end;

//procedure setHdObjectClassProp(AObject: TObject; ApropName: string; AValue: TClass);
//begin
//  if IsReadOnlyProp(Aobject, APropName) then
//  begin
//    PClass(ptrUint(Aobject) + (ptrUint(GetPropInfo(Aobject, APropName)^.GetProc) and
//      $00FFFFFF))^ := AValue;
//  end
//  else
//    typInfo.seto (AObject, APropName, AValue);
//end;

function getHdObjectClassProp(AObj: TClass; APropName: string): TClass;
begin
  Result := typinfo.GetObjectPropClass(AObj, APropName);
end;

function getHdObjectClassProp(AObj: TObject; APropName: string): TClass;
begin
 Result := typinfo.GetObjectPropClass(AObj, APropName);
end;

end.
