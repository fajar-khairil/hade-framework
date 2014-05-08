//this unit come from http://delphi.about.com/library/weekly/aa022205a.htm
unit hdguid;

{$mode objfpc}{$H+}
interface

uses SysUtils;

type

  { TGuidEx }

  TGuidEx = class
    //Creates and returns a new globally unique identifier
    class function NewGuid : TGuid;
    //sometimes we need to have an "empty" value, like NULL
    class function EmptyGuid : TGuid;
    //Checks whether a Guid is EmptyGuid
    class function IsEmptyGuid(Guid : TGuid) : boolean;
    //Convert to string
    class function ToString(Guid : TGuid) : string;
    //convert to quoted string
    class function ToQuotedString(Guid : TGuid) : string;
    //return a GUID from string
    class function FromString(Value : string) : TGuid;
    //Indicates whether two TGUID values are the same
    class function EqualGuids(Guid1, Guid2 : TGuid) : boolean;

    class function NewGuidAsString:string;
  end;

implementation

{ TGuidEx }

class function TGuidEx.EmptyGuid: TGuid;
begin
  result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function TGuidEx.EqualGuids(Guid1, Guid2: TGuid): boolean;
begin
  result := IsEqualGUID(Guid1, Guid2);
end;

class function TGuidEx.NewGuidAsString: string;
var
  oguid: TGUID;
  tmp: String;
begin
  oguid:= TGuidEx.NewGuid;
  tmp := TGuidEx.ToString(oguid);
  Result:= tmp;
end;

class function TGuidEx.FromString(Value: string): TGuid;
begin
  result := StringToGuid(Value);
end;

class function TGuidEx.IsEmptyGuid(Guid : TGuid): boolean;
begin
  result := EqualGuids(Guid,EmptyGuid);
end;

class function TGuidEx.NewGuid: TGuid;
var
  Guid : TGuid;
begin
  CreateGUID(Guid);
  Result := Guid;
end;

class function TGuidEx.ToQuotedString(Guid: TGuid): string;
begin
  result := QuotedStr(ToString(Guid));
end;

class function TGuidEx.ToString(Guid: TGuid): string;
begin
  result := GuidToString(Guid);
end;

end.//GuidEx
