unit hdimplementormanager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdbase,
  hdmapbase,
  hdimplementorintf,
  hdbroker,
  hdobject;

type

  EHadeImplManagerException = class(EHadeException);

  { THadeImplementorInfo }

  THadeImplementorInfo = class
  private
    FImplementorBroker: THadeBroker;
    FImplementorClass: THadeImplementorClass;
    FImplementorName: string;
    FIsDefault: boolean;
    FObjectClassName: string;
  public
    property ImplementorName: string read FImplementorName;
    property ObjectClassName: string read FObjectClassName;
    property ImplementorClass: THadeImplementorClass read FImplementorClass;
    property ImplementorBroker: THadeBroker read FImplementorBroker;
    property IsDefault: boolean read FIsDefault;

    constructor Create(AImplName: string; AObjectClass: THadeObjectClass;
      AImplClasses: THadeImplementorClass; ABroker: THadeBroker;
      ADefault: boolean = False);
  end;

  { THadeImplementorManager }

  THadeImplementorManager = class(THadeBaseObject)
  protected
    FList: THadeMapBase;
    procedure RegisterDefaultImplementor;
  public
    procedure RegisterImplementor(AImplName: string; AObjectClass: THadeObjectClass;
      AImplClasses: THadeImplementorClass; ABroker: THadeBroker;
      ADefault: boolean = False);
    procedure UnRegisterImplementor(AImplName: string);

    function GetByName(AImplName: string): THadeImplementorClass;
    function GetByClassName(AClassName: string): THadeImplementorClass;
    function GetDefaultImplementorFor(ABroker: THadeBroker): THadeImplementorClass;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  hdsqldbimplementor;

{ THadeImplementorInfo }

constructor THadeImplementorInfo.Create(AImplName: string;
  AObjectClass: THadeObjectClass; AImplClasses: THadeImplementorClass;
  ABroker: THadeBroker; ADefault: boolean);
begin
  Self.FImplementorName := AImplName;
  if AObjectClass <> nil then
    Self.FObjectClassName := AObjectClass.ClassName
  else
    Self.FObjectClassName := '';
  Self.FImplementorClass := AImplClasses;
  Self.FImplementorBroker := ABroker;
  FIsDefault := ADefault;
end;

procedure THadeImplementorManager.RegisterDefaultImplementor;
begin
  Self.RegisterImplementor('DefSQLDBFirebird', nil, THadeSQLDBImplementor,
    SQLDBFirebird, True);
  Self.RegisterImplementor('DefSQLDBSQlite', nil, THadeSQLDBImplementor, SQLDBSQLite, True);
end;

procedure THadeImplementorManager.RegisterImplementor(AImplName: string;
  AObjectClass: THadeObjectClass; AImplClasses: THadeImplementorClass;
  ABroker: THadeBroker; ADefault: boolean);
begin
  Flist.add(AImplName, THadeImplementorInfo.Create(
    AImplName, AObjectClass, AImplClasses, ABroker, ADefault));
end;

procedure THadeImplementorManager.UnRegisterImplementor(AImplName: string);
var
  idx: integer;
begin
  idx := Flist.FindIndexOf(AImplName);
  if idx > 0 then
  begin
    FList.Delete(idx);
    FList.Pack;
  end;
end;

function THadeImplementorManager.GetByName(AImplName: string): THadeImplementorClass;
begin
  Result := THadeImplementorInfo(FList.Find(AImplName)).ImplementorClass;
  if not Assigned(Result) then
    raise EHadeImplManagerException.Create('Connot find implementor ' + AImplName);
end;

function THadeImplementorManager.GetByClassName(AClassName: string):
THadeImplementorClass;
var
  iloop: integer;
  impl: THadeImplementorInfo;
begin
  Result := nil;

  for iloop := 0 to pred(FList.Count) do
  begin
    impl := THadeImplementorInfo(FList.Items[iloop]);
    if SysUtils.AnsiCompareText(impl.ObjectClassName, AClassName) = 0 then
    begin
      Result := impl.ImplementorClass;
      Break;
    end;
  end;

end;

function THadeImplementorManager.GetDefaultImplementorFor(ABroker: THadeBroker):
THadeImplementorClass;
var
  iloop: integer;
  impl: THadeImplementorInfo;
  Found: boolean;
begin
  Found := False;

  for iloop := 0 to pred(FList.Count) do
  begin
    impl := THadeImplementorInfo(FList.Items[iloop]);
    if (impl.ImplementorBroker = ABroker) and (impl.IsDefault) then
    begin
      Result := impl.ImplementorClass;
      Found := True;
      Break;
    end;
  end;

  if not Found then
    raise EHadeImplManagerException.Create(
      'no default implemontor found for broker ' + hdbroker.BrokerAsString(ABroker));
end;

constructor THadeImplementorManager.Create;
begin
  FList := THadeMapBase.Create();
  Self.RegisterDefaultImplementor;
end;

destructor THadeImplementorManager.Destroy;
begin
  FList.Clear;
  FList.Free;
  inherited Destroy;
end;

end.

