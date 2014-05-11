program dicontainer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  hade.DiContainer
  { you can add units after this };
Type

{ TPerson }

TPerson = class
private
  FName: string;
public
  property Name : string read FName write FName;
  procedure sayHello();
end;

{ TObjectFactory }

TObjectFactory = class
protected
  FCounter : ptrUint;
  procedure inCounter();
public
  function getCounter : ptrUint;

  procedure testMake(out APerson: Pointer);

  constructor Create;
end;

{ TTest }

procedure TObjectFactory.inCounter;
begin
  inc(FCounter);
end;

function TObjectFactory.getCounter: ptrUint;
begin
  Result := FCounter;
end;

procedure TObjectFactory.testMake(out APerson: Pointer);
begin
  WriteLn('-------------------------------');
  WriteLn('Debug : Creating Person Object');
  WriteLn('-------------------------------');
  APerson := TPerson.Create;
  TPerson(APerson).Name := 'Fajar';
  inCounter();
end;

constructor TObjectFactory.Create;
begin
  FCounter := 0;
end;

{ TTest }

procedure TPerson.sayHello;
begin
  WriteLn('Hello '+self.Name);
end;

var
  GContainer :TDIContainer;
  Obj: TPerson;
  obj2: TPerson;
  factory: TObjectFactory;

begin
  factory := TObjectFactory.Create;
  GContainer := TDIContainer.Create();
  try
    GContainer.Bind('test',@factory.testMake);
    GContainer.Singleton('singleton',@factory.testMake);

    TPerson(GContainer.make('singleton')).sayHello();
    TPerson(GContainer.make('singleton')).sayHello();

    obj2 := TPerson(GContainer.make('test'));
    Obj := TPerson(GContainer.make('test'));
    obj2.sayHello();
    obj.sayHello();
    WriteLn('--------------END-------------');
    WriteLn(GContainer.UnitName);
    WriteLn('The Size of DIContainer instance : '+IntToStr(GContainer.InstanceSize));
    WriteLn('Factory Counter called : ',Factory.getCounter);
  finally
    Obj.Free;
    Obj2.Free;
    factory.Free;
    GContainer.Free;
  end;
end.

