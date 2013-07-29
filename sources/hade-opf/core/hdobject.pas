unit hdobject;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdbase,
  contnrs,
  fpjson,
  hdcriteria;

type

  EHadeObjectException = class(EHadeException);
  THadeObjectState = (
    posEmpty,
    posPK,
    posCreate,
    posUpdate,
    posDelete,
    posDeleted,
    posClean
    );

  THadeCustomObject = class;

  {$INTERFACES CORBA}

  { IHadeCustomObject }

  IHadeCustomObject = interface
    ['{0676D026-BCFE-40AD-8B49-301887AE34B3}']
    function getOwner: THadeCustomObject;
    function addChild(AObj: THadeCustomObject): ptrint;
    property Owner: THadeCustomObject read getOwner;
  end;

  IHadeJSONSerializer = interface
    ['{019D9B23-C8D5-4C70-8225-D03E67AB7209}']
    procedure FromJson(AJSonObject: TJSonObject);
    procedure ToJSON(out AJSonObject: TJSONObject; AIncludeClassName: boolean = True);
    function ToJSONString(AInCludeClassName: boolean = True): string;
    procedure LoadFromJSON(const AFileName: string);
    procedure SaveAsJSON(const AFileName: string);
  end;

  { IHadeObject }

  IHadeObject = interface(IHadeJSONSerializer)
    ['{4E5CF5F5-1F82-432E-ACC5-62C474084DB9}']
    function getState: THadeObjectState;
    property ObjectState: THadeObjectState read getState;
    procedure SetState(const AValue: THadeObjectState);

    procedure MarkDelete;
    procedure MarkDirty;
    procedure MarkClean;
    procedure MarkModified;
  end;

  { THadeCustomObject }

  THadeCustomObject = class(THadeBaseObject, IHadeCustomObject, IHadeJSONSerializer)
  protected
    fOID: string;
    fOwner: THadeCustomObject;
    fList: TFpObjectList;//maintaining childs

    function getOwner: THadeCustomObject; reintroduce;
    procedure ClearChilds;
    function addChild(AObj: THadeCustomObject): ptrint;
    procedure RemoveChild(AObject: THadeCustomObject);

    procedure RaiseError(const AMsg: string);
  public
    property Owner: THadeCustomObject read getOwner;

    //json function
    procedure FromJson(AJSonObject: TJSonObject); virtual;
    procedure LoadFromJSON(const AFileName: string); virtual;
    function ToJSONString(AInCludeClassName: boolean = True): string;
    procedure ToJSON(out AJSonObject: TJSONObject;
      AIncludeClassName: boolean = True); virtual;
    procedure SaveAsJSON(const AFileName: string); virtual;
    procedure Clone(ObjTo: THadeCustomObject);

    constructor Create(AOwner: THadeCustomObject); virtual;
    destructor Destroy; override;
  published
    property OID: string read FOID;
  end;

  THadeCustomObjectClass = class of THadeCustomObject;

  THadeObjectList = class;

  { THadeObject }

  THadeObject = class(THadeCustomObject, IHadeObject)
  protected
    FState: THadeObjectState;
    procedure MarkClean;
    procedure MarkDirty;
    function getState: THadeObjectState;
    procedure SetState(const AValue: THadeObjectState);
  public
    procedure FromJson(AJSonObject: TJSonObject); override;
    procedure LoadFromJSON(const AFileName: string); override;

    property ObjectState: THadeObjectState read getState;
    procedure MarkDelete;
    procedure MarkModified;

    constructor Create(AOwner: THadeCustomObject); override;
  end;

  THadeObjectClass = class of THadeObject;

  { IHadeObjectList }
  {$INTERFACES CORBA}
  IHadeObjectList = interface(IHadeCustomObject)
    ['{930FBD9F-904C-4D19-BB15-A0A4750A2CBD}']
    function GetCriteria: THadeCriteria;
    function getItem(AIndex: ptrUint): THadeObject;
    procedure setItem(AIndex: ptrUint; AValue: THadeObject);
    property Items[AIndex: ptrUint]: THadeObject read getItem write setItem; default;
    function Count: ptrUint;
    procedure Clear;
    function Add(AObject: THadeObject): ptrUint;
    procedure Remove(AObject: THadeObject);
    procedure Delete(AIndex: ptrUint);

    property Criteria:THadeCriteria read GetCriteria;
  end;

  { THadeObjectList }

  THadeObjectList = class(THadeCustomObject, IHadeObjectList)
  private
    class var FChildClass: THadeObjectClass;
  protected
    FObjList: TFpObjectList;
    FCriteria: THadeCriteria;

    function getItem(AIndex: ptrUint): THadeObject;
    procedure setItem(AIndex: ptrUint; AValue: THadeObject);

    class function getChilClass: THadeObjectClass; static;
    function GetCriteria: THadeCriteria;
  public
    class property ChildClass: THadeObjectClass read getChilClass write FChildClass;

    property Items[AIndex: ptrUint]: THadeObject read getItem write setItem; default;
    function Add(AObject: THadeObject): ptrUint;
    procedure Remove(AObject: THadeObject);
    procedure Delete(AIndex: ptrUint);
    function Count: ptrUint;
    procedure Clear;

    //json functions
    procedure FromJson(AJSonObject: TJSonObject); override;
    function ToJSONString(AInCludeClassName: boolean = True): string;
    procedure ToJSON(out AJSonObject: TJSONObject;
      AIncludeClassName: boolean = True); override;
    procedure LoadFromJSON(const AFileName: string); override;
    procedure SaveAsJSON(const AFileName: string); override;

    property Criteria:THadeCriteria read GetCriteria;

    constructor Create(AOwner: THadeCustomObject); override;
    destructor Destroy; override;
  end;

  THadeObjectListClass = class of THadeObjectList;

implementation

uses
  hdrtti,
  typinfo,
  jsonparser,
  hdguid;

{ THadeCustomObject }

function THadeCustomObject.getOwner: THadeCustomObject;
begin
  Result := FOwner;
end;

procedure THadeCustomObject.RemoveChild(AObject: THadeCustomObject);
begin
  if self.Equals(Aobject.Owner) then
    AObject.Free;
  fList.Remove(AObject);
end;

procedure THadeCustomObject.RaiseError(const AMsg: string);
begin
  raise EHadeObjectException.Create(AMsg);
end;

procedure THadeCustomObject.ClearChilds;
var
  iloop: integer;
begin
  for iloop := 0 to pred(FList.Count) do
    if self.Equals(THadeObject(FList.Items[iloop]).Owner) then
      FList.Items[iloop].Free;

  FList.Clear;
end;

function THadeCustomObject.addChild(AObj: THadeCustomObject): ptrint;
begin
  Result := FList.Add(AObj);
end;

procedure THadeCustomObject.FromJson(AJSonObject: TJSonObject);
var
  props: TStringList;
  jloop: integer;
  el: TJSONData;
begin
  props := hdrtti.GetPropInfoList(self);

  try
    for jloop := 0 to pred(props.Count) do
    begin
      el := AJsonObject.Find(props[jloop]);

      if hdrtti.IsStringProp(self, props[jloop]) then
        hdrtti.setStringProp(self, props[jloop], el.AsString);

      if hdrtti.IsIntegerProp(self, props[jloop]) then
        hdrtti.setIntegerProp(self, props[jloop], el.AsInteger);

      case typinfo.getPropInfo(self, props[jloop])^.PropType^.Kind of
        tkEnumeration:
          hdrtti.setOrdinalProp(self, props[jloop], el.AsInteger);
        tkFloat:
        begin
          if getPropInfo(self, props[jloop])^.PropType^.Name = 'TDateTime' then
            hdrtti.setFloatProp(self, props[jloop],
              SysUtils.StrToDateTime(el.AsString))
          else
            hdrtti.setFloatProp(self, props[jloop], el.AsFloat);
        end;
        tkVariant:
          hdrtti.setVarProp(self, props[jloop], el.Value);
        tkBool:
          hdrtti.setOrdinalProp(self, props[jloop], el.AsInteger);
      end;
    end;
  finally
    FreeAndNil(props);
  end;
end;

procedure THadeCustomObject.Clone(ObjTo: THadeCustomObject);
var
  PropInfos: PPropList;
  PropInfo: PPropInfo;
  Count, Loop: Integer;
  OrdVal: Longint;
  StrVal: String;
  FloatVal: Extended;
  //MethodVal: TMethod;
  oldClass: TClass;
begin
  { Iterate thru all published fields and properties of source }
  { copying them to target }

  { Find out how many properties we'll be considering }
  Count := GetPropList(Self.ClassInfo, tkAny, nil);
  { Allocate memory to hold their RTTI data }
  GetMem(PropInfos, Count * SizeOf(PPropInfo));
  try
    { Get hold of the property list in our new buffer }
    GetPropList(Self.ClassInfo, tkAny, PropInfos);
    { Loop through all the selected properties }
    for Loop := 0 to Count - 1 do
    begin
      PropInfo := GetPropInfo(ObjTo.ClassInfo, PropInfos^[Loop]^.Name);
      { Check the general type of the property }
      { and read/write it in an appropriate way }
      case PropInfos^[Loop]^.PropType^.Kind of
        tkChar, tkEnumeration,
        tkSet, tkWChar:
        begin
           OrdVal := hdrtti.GetOrdinalProp(Self, PropInfo^.Name);
          if Assigned(PropInfo) then
            hdrtti.SetOrdinalProp(ObjTo,PropInfo^.Name, OrdVal);
        end;
        tkInteger,tkInt64 :
        begin
          OrdVal := hdrtti.GetIntegerProp(Self, PropInfo^.Name);
          if Assigned(PropInfo) then
            hdrtti.SetIntegerProp(ObjTo,PropInfo^.Name, OrdVal);
        end;
        tkFloat:
        begin
          FloatVal := hdrtti.GetFloatProp(Self, PropInfo^.Name);
          if Assigned(PropInfo) then
            hdrtti.SetFloatProp(ObjTo,PropInfo^.Name, FloatVal);
        end;
        tkWString,
        tkLString,
        tkString:
        begin
          { Avoid copying 'OID' - components must have unique OID }
          if UpperCase(PropInfos^[Loop]^.Name) = 'OID' then
            Continue;
          StrVal := hdrtti.GetStringProp(Self, PropInfo^.Name);
          if Assigned(PropInfo) then
            hdrtti.SetStringProp(ObjTo,PropInfo^.Name, StrVal);
        end;
        tkClass:
        begin
          oldClass:= hdrtti.getHdObjectClassProp(ObjTo,PropInfo^.Name);
          ObjTo.Clone(THadeObjectClass(oldClass).Create(THadeCustomObject(ObjTo)));
        end;
        {tkMethod:
        begin
          MethodVal := GetMethodProp(ObjFrom, PropInfos^[Loop]);
          if Assigned(PropInfo) then
            SetMethodProp(ObjTo, PropInfo, MethodVal);
        end}
      end
    end
  finally
    FreeMem(PropInfos, Count * SizeOf(PPropInfo));
  end;
end;

function THadeCustomObject.ToJSONString(AInCludeClassName: boolean): string;
var
  fjson: TJSONObject;
begin
  try
    self.ToJSON(fjson, AIncludeClassName);
    Result := fjson.AsJSON;
  finally
    fjson.Free;
  end;
end;

procedure THadeCustomObject.ToJSON(out AJSonObject: TJSONObject;
  AIncludeClassName: boolean);
var
  props: TStringList;
  jloop: integer;
begin
  AJSonObject := TJSONObject.Create;
  if AIncludeClassName then
    AJSonObject.Add('Class', self.ClassName);
  props := hdrtti.GetPropInfoList(self);
  try
    for jloop := 0 to pred(props.Count) do
    begin
      if hdrtti.IsStringProp(self, props[jloop]) then
        AJSonObject.Add(props[jloop], hdrtti.getStringProp(self, props[jloop]));

      if hdrtti.IsIntegerProp(self, props[jloop]) then
        AJSonObject.Add(props[jloop], hdrtti.getIntegerProp(self, props[jloop]));

      case typinfo.getPropInfo(self, props[jloop])^.PropType^.Kind of
        tkEnumeration:
          AJSonObject.Add(props[jloop], hdrtti.getIntegerProp(self, props[jloop]));
        tkFloat:
          if getPropInfo(self, props[jloop])^.PropType^.Name = 'TDateTime' then
            AJSonObject.Add(props[jloop], FormatDateTime(
              'dd-mm-yyyy', hdrtti.getFloatProp(self, props[jloop])))
          else
            AJSonObject.Add(props[jloop], hdrtti.getFloatProp(self, props[jloop]));
        tkVariant:
          AJSonObject.Add(props[jloop], hdrtti.getStringProp(self, props[jloop]));
        tkBool:
          AJSonObject.Add(props[jloop], hdrtti.getOrdinalProp(self, props[jloop]));
      end;
    end;
  finally
    FreeAndNil(props);
  end;
end;

procedure THadeCustomObject.LoadFromJSON(const AFileName: string);
var
  fileJson: TStringList;
  jsonObj: TJSONObject;
  parser: TJSONParser;
begin
  filejson := TStringList.Create;
  try
    try
      filejson.LoadFromFile(AFileName);
      parser := TJSONParser.Create(filejson.Text);
      jsonobj := parser.Parse as TJSONObject;
      if Assigned(jsonobj) then
        self.FromJson(jsonobj);
    except
      on E: Exception do
        self.RaiseError('Failed to load json, reason : ' + LineEnding + E.Message);
    end;
  finally
    filejson.Free;
    jsonObj.Free;
    parser.Free;
  end;
end;

procedure THadeCustomObject.SaveAsJSON(const AFileName: string);
var
  fileJson: TStringList;
  jsonObj: TJSONObject;
begin
  fileJson := TStringList.Create;
  try
    self.ToJSON(jsonObj);
    filejson.Text := jsonobj.AsJSON;
    filejson.SaveToFile(AFileName);
  finally
    filejson.Free;
    jsonobj.Free;
  end;
end;

constructor THadeCustomObject.Create(AOwner: THadeCustomObject);
begin
  fOID := TGuidEx.NewGuidAsString;
  FList := TFpObjectList.Create(False);
  fOwner := AOwner;
  if Assigned(FOwner) then
    (FOwner as IHadeCustomObject).addChild(self);
end;

destructor THadeCustomObject.Destroy;
begin
  ClearChilds();//free all childs
  FList.Free;
  inherited Destroy;
end;

{ THadeObjectList }

class function THadeObjectList.getChilClass: THadeObjectClass; static;
begin
  if not Assigned(FChildClass) then
    raise EHadeObjectException.Create('you must set ChildClass property');

  Result := FChildClass;
end;

function THadeObjectList.GetCriteria: THadeCriteria;
begin
  if not Assigned(FCriteria)then
    FCriteria:= THadeCriteria.Create(self);
  Result:= FCriteria;
end;

function THadeObjectList.getItem(AIndex: ptrUint): THadeObject;
begin
  Result := THadeObject(FObjList.Items[AIndex]);
end;

procedure THadeObjectList.setItem(AIndex: ptrUint; AValue: THadeObject);
begin
  FObjList.Items[AIndex] := AValue;
end;

function THadeObjectList.Count: ptrUint;
begin
  Result := FObjList.Count;
end;

procedure THadeObjectList.Clear;
begin
  FObjList.Clear;
end;

procedure THadeObjectList.FromJson(AJSonObject: TJSonObject);
var
  iloop: integer;
  obj: THadeObject;
  objects: TJSONArray;
begin
  self.Clear;
  try
    objects := TJSONArray(AJSonObject.Arrays['Objects']);//dont localize
  except
    RaiseError('not compatible json structure.');
  end;

  for iloop := 0 to pred(objects.Count) do
  begin
    obj := FChildClass.Create(self);
    obj.FromJson(TJSONObject(objects.Items[iloop]));
    self.Add(obj);
  end;
end;

function THadeObjectList.ToJSONString(AInCludeClassName: boolean): string;
var
  fjson: TJSONObject;
begin
  try
    self.ToJSON(fjson, AIncludeClassName);
    Result := fjson.AsJSON;
  finally
    fjson.Free;
  end;
end;

procedure THadeObjectList.ToJSON(out AJSonObject: TJSONObject;
  AIncludeClassName: boolean);
var
  iloop: integer;
  js: TJSONObject;
begin
  AJSonObject := TJSONObject.Create;
  AJSonObject.Add('Generator', self.version);
  if AIncludeClassName then
    AJsonObject.Add('Class', FChildClass.ClassName);
  AJSONObject.Add('Objects', TJSONArray.Create);

  for iloop := 0 to pred(self.Count) do
  begin
    self.Items[iloop].ToJSON(js, False);
    AJSonObject.Arrays['Objects'].Add(js);
  end;
end;

procedure THadeObjectList.LoadFromJSON(const AFileName: string);
var
  fileJson: TStringList;
  jsonObj: TJSONObject;
  parser: TJSONParser;
begin
  fileJson := TStringList.Create;
  try
    try
      filejson.LoadFromFile(AFileName);
      parser := TJSONParser.Create(filejson.Text);
      jsonobj := parser.Parse as TJSONObject;
      self.FromJson(jsonobj);
    except
      on E: Exception do
        RaiseError('Failed to load json, reason ' + LineEnding + E.Message);
    end;
  finally
    filejson.Free;
    jsonobj.Free;
    parser.Free;
  end;
end;

procedure THadeObjectList.SaveAsJSON(const AFileName: string);
var
  fjson: TJSONObject;
  fileJson: TStringList;
begin
  self.ToJSON(fjson, True);
  filejson := TStringList.Create;
  try
    filejson.Text := fjson.AsJSON;
    filejson.SaveToFile(AFileName);
  finally
    filejson.Free;
    fjson.Free;
  end;
end;

function THadeObjectList.Add(AObject: THadeObject): ptrUint;
begin
  Result := FObjList.Add(AObject);
  AObject.Tag := Result;
end;

procedure THadeObjectList.Remove(AObject: THadeObject);
begin
  FObjList.Remove(AObject);
  if self.Equals(AObject.Owner) then
    self.RemoveChild(AObject);
end;

procedure THadeObjectList.Delete(AIndex: ptrUint);
begin
  if self.Equals(Items[AIndex].Owner) then
    self.RemoveChild(Items[AIndex]);

  FObjList.Delete(AIndex);
end;

constructor THadeObjectList.Create(AOwner: THadeCustomObject);
begin
  inherited Create(AOwner);
  FObjList := TFpObjectList.Create(False);
end;

destructor THadeObjectList.Destroy;
begin
  fobjlist.Free;
  if Assigned(FCriteria) then
    FCriteria.Free;
  inherited Destroy;
end;

{ THadeObject }

procedure THadeObject.MarkDirty;
begin
  case FState of
    posEmpty: FState := posCreate;
    posDeleted: FState := posCreate;
  end;
end;

procedure THadeObject.MarkClean;
begin
  case FState of
    posDelete: FState := posDeleted;
    posDeleted: FState := posCreate;
    else
      Fstate := posClean;
  end;
end;

procedure THadeObject.MarkDelete;
begin
  if FState in [posUpdate, posClean] then
    FState := posDelete;
end;

procedure THadeObject.MarkModified;
begin
  FState:= posUpdate;
end;

function THadeObject.getState: THadeObjectState;
begin
  Result := FState;
end;

procedure THadeObject.SetState(const AValue: THadeObjectState);
begin
  Self.FState := AValue;
end;

procedure THadeObject.FromJson(AJSonObject: TJSonObject);
begin
  inherited FromJson(AJSonObject);
  Self.MarkDirty;;
end;

procedure THadeObject.LoadFromJSON(const AFileName: string);
begin
  inherited LoadFromJSON(AFileName);
  Self.MarkDirty;
end;

constructor THadeObject.Create(AOwner: THadeCustomObject);
begin
  inherited Create(AOwner);
  FState := posEmpty;
end;

end.
