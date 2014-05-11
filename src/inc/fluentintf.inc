{ EFluent }

EFluent = class(Exception)End;


//fpc generic interface is still buggy, take clasic approach
{$INTERFACES CORBA}
generic IGenericFluentEnumerator<T> = interface
['{3133BED5-AC73-4419-94C3-4691F4528DDC}']
  function GetCurrent:T;
  function MoveNext: Boolean;
  procedure Reset;
  property Current: T read GetCurrent;
end;

{ TCustomFluent }

generic TCustomFluent<T> = class(specialize IGenericFluentEnumerator <T>)
private
  function getCount: ptrUint;
protected
  FCursor : ptrUint;
  FKeyList : TStringList;
  FList : TStringHashMap;

  function GetCurrent: T;
public
  procedure clear;
  function First: T;
  function Last: T;
  function Extract(const ItemKey:shortstring): T;
  procedure Remove(const ItemKey:shortstring);

  property List : TStringHashMap read FList;
  property Count : ptrUint read getCount;
  //IEnumerator
  function MoveNext: Boolean;
  procedure Reset;
  property Current: T read GetCurrent;

  function Items(const AKeyName:shortstring) : T;

  constructor Create;
  Destructor Destroy;override;
end;
