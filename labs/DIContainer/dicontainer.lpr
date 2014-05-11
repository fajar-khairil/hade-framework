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
  procedure testMake(out APerson: Pointer);
  procedure testMakeSingleton(out APerson: Pointer);
end;

{ TTest }

procedure TPerson.testMake(out APerson: Pointer);
begin
  WriteLn('-------------------------------');
  WriteLn('Debug : Creating Person Object');
  WriteLn('-------------------------------');
  APerson := TPerson.Create;
  TPerson(APerson).Name := 'Fajar';
end;

procedure TPerson.testMakeSingleton(out APerson: Pointer);
begin
  WriteLn('-------------------------------');
  WriteLn('Debug : Creating Singleton Object');
  WriteLn('-------------------------------');
  APerson := TPerson.Create;
  TPerson(APerson).Name := 'Fajar Singleton';
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
  test: TPerson;

begin
  test := TPerson.Create;
  GContainer := TDIContainer.Create();
  try
    GContainer.Bind('test',@test.testMake);
    GContainer.Singleton('singleton',@test.testMakeSingleton);

    TPerson(GContainer.make('singleton')).sayHello();
    TPerson(GContainer.make('singleton')).sayHello();

    obj2 := TPerson(GContainer.make('test'));
    Obj := TPerson(GContainer.make('test'));
    obj2.sayHello();
    obj.sayHello();
    WriteLn('--------------END-------------');
    WriteLn(GContainer.UnitName);
    WriteLn('The Size of DIContainer instance : '+IntToStr(GContainer.InstanceSize));
  finally
    Obj.Free;
    Obj2.Free;
    test.Free;
    GContainer.Free;
  end;
end.

