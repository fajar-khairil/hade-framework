unit hdmapbase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  hdbase;
Type
  EHadeMapBase = EHadeException;
  { THadeMapBase }

  THadeMapBase = class(TFPHashObjectList)
  private
    fupper: boolean;
  protected
    function IsKeyExists(const aKey: string): boolean;
  public
    function Find(const s: shortstring): TObject;
    function add(const AName: shortstring; const AObject: TObject;
      const FreeIfExists: boolean = True): integer; virtual;
    function addReplace(const AName: shortstring; const AObject: TObject): integer;
    procedure uppercaseKey(aUpper: boolean);

    constructor Create(FreeObjects: boolean = True); virtual;
  end;

implementation

{ THadeMapBase }

function THadeMapBase.IsKeyExists(const aKey: string): boolean;
begin
  Result := False;
  if Find(AKey) <> nil then
    Result := True;
end;

function THadeMapBase.add(const AName: shortstring; const AObject: TObject;
  const FreeIfExists: boolean): integer;
begin
  Result := -1;

  if IsKeyExists(AName) then
  begin
    if FreeIfExists then
    begin
      AObject.Free;
      Exit;//==>
    end;
  end;

  if fupper then
    Result := inherited Add(UpperCase(AName), AObject)
  else
    Result := inherited Add(AName, AObject);
end;

function THadeMapBase.addReplace(const AName: shortstring;
  const AObject: TObject): integer;
var
  idx: integer;

begin
  Result := -1;

  if IsKeyExists(AName) then
  begin
    idx := self.IndexOf(self.Find(AName));
    self.Delete(idx);
  end;

  if fupper then
    Result := inherited Add(UpperCase(AName), AObject)
  else
    Result := inherited Add(AName, AObject);
end;

function THadeMapBase.Find(const s: shortstring): TObject;
begin
  if fupper then
    Result := inherited find(UpperCase(s))
  else
    Result := inherited find(s);

  //if Result = nil then
    //Raise EHadeMapBase.Create(self.ClassName+' cannot find '+s);
end;

procedure THadeMapBase.uppercaseKey(aUpper: boolean);
begin
  fupper := aUpper;
end;

constructor THadeMapBase.Create(FreeObjects: boolean);
begin
  inherited Create(FreeObjects);
  fupper := True;
end;

end.

