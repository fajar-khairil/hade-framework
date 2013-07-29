unit hdimplementorintf;

{$mode objfpc}{$H+}

interface
uses
  sysutils,
  hdbase,
  hdconnection,
  hdpersistentintf,
  hdobjectfactory,
  hdquery;
type
  TFetchMode = hdpersistentintf.TFetchMode;
  { IHadeImplementor }

  IHadeImplementor = interface
    ['{FB645E1D-49DC-4983-9297-E70DC38A8BBF}']
    function GetObjectFactory: THadeObjectFactory;
    function GetQuery: THadeQuery;

    property Query:THadeQuery read GetQuery;
    property ObjectFactory : THadeObjectFactory read GetObjectFactory;

    procedure Update(AObject: TObject);
    procedure Insert(AObject: TObject);
    procedure Delete(AObject: TObject);
    procedure Select(AObject: TClass;AFetchMode: TFetchMode);
  end;

  { EHadeProccessorException }

  EHadeImplentorException = class(EHadeException);
  { THadeCustomImplementor }

  THadeCustomImplementor = class(TInterfacedObject,IHadeImplementor)
  protected
    FConnection:THadeConnection;
    FObjectFactory:THadeObjectFactory;
    FQuery:THadeQuery;

    function GetObjectFactory: THadeObjectFactory;
    function GetQuery: THadeQuery;
    function CreateQuery: THadeQuery;virtual;
    function CreateObjectFactory :THadeObjectFactory;virtual;

    procedure RaiseError(const AMsg:string);
  public
    procedure Update(AObject: TObject);virtual;
    procedure Insert(AObject: TObject);virtual;
    procedure Delete(AObject: TObject);virtual;
    procedure Select(AObject: TClass; AFetchMode: TFetchMode);virtual;

    property Query:THadeQuery read GetQuery;
    property ObjectFactory : THadeObjectFactory read GetObjectFactory;

    constructor Create(AConnection:THadeConnection);virtual;
    destructor Destroy;override;
  end;

  THadeImplementorClass = Class of THadeCustomImplementor;

implementation
uses
  hdobject;
{ THadeCustomImplementor }

function THadeCustomImplementor.GetObjectFactory: THadeObjectFactory;
begin
  Result:= FObjectFactory;
end;

function THadeCustomImplementor.GetQuery: THadeQuery;
begin
  Result:= FQuery;
end;

function THadeCustomImplementor.CreateQuery: THadeQuery;
begin
  Result := THadeQuery.Create(FConnection);
end;

function THadeCustomImplementor.CreateObjectFactory: THadeObjectFactory;
begin
  Result:= THadeObjectFactory.Create(FQuery);
end;

procedure THadeCustomImplementor.RaiseError(const AMsg: string);
begin
  raise EHadeImplentorException.Create(AMsg);
end;

procedure THadeCustomImplementor.Update(AObject: TObject);
begin
  if not sysutils.supports(AObject,IHadeObject) then
    Self.RaiseError('Unsuported Object.');
end;

procedure THadeCustomImplementor.Insert(AObject: TObject);
begin
  if not sysutils.supports(AObject,IHadeObject) then
    Self.RaiseError('Unsuported Object.');
end;

procedure THadeCustomImplementor.Delete(AObject: TObject);
begin
  if not sysutils.supports(AObject,IHadeObject) then
    Self.RaiseError('Unsuported Object.');
end;

procedure THadeCustomImplementor.Select(AObject: TClass; AFetchMode: TFetchMode
  );
begin
  if not sysutils.supports(AObject,IHadeObject) then
    Self.RaiseError('Unsuported Object.');
end;

constructor THadeCustomImplementor.Create(AConnection: THadeConnection);
begin
  FConnection:= AConnection;
  FQuery := CreateQuery;
  FObjectFactory := CreateObjectFactory;
end;

destructor THadeCustomImplementor.Destroy;
begin
  if Assigned(FObjectFactory) then FObjectFactory.Free;
  if Assigned(FQuery) then FQuery.Free;
  inherited Destroy;
end;

end.
