{ EFluent }

EFluent = class(Exception)End;

{ TCustom  Fluent }

{ TCustomFluent }

generic TCustomFluent<T> = class
protected
  FCursor : ptrUint;
  FKeyList : TStringList;
  FList : TStringHashMap;
  function getCount: ptrUint;
public
  procedure clear;
  procedure Remove(const ItemKey:shortstring);

  property Map : TStringHashMap read FList;
  property PropertyCount : ptrUint read getCount;
  property Count : ptrUint read getCount;

  function Items(const AKeyName:shortstring) : T;

  constructor Create;
  Destructor Destroy;override;
end;
