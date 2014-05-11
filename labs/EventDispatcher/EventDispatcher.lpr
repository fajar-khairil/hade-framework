program EventDispatcher;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  hade.eventdispatcher,
  hade.eventdispatcherintf;

Type

  { TWomen }

  TWomen = class
  public
    procedure greeting();
  end;

  { TPerson }

  TPerson = class
  private
    FAge: ptrUint;
    FName: string;
  public
    property Name: string read FName write FName;
    property Age : ptrUint read FAge write FAge;

    procedure sayHello(Sender:Pointer);
  end;

{ TJustTest }

TJustTest = class
procedure fire();
end;

var
  GEventDispatcher : TEventDispatcher ;

{ TWomen }

procedure TWomen.greeting;
begin
  WriteLn('Welcome to Freepascal world!!!');
end;

procedure TPerson.sayHello(Sender:Pointer);
begin
  WriteLn('Hello '+self.Name+'!!!');
  WriteLn('Age : '+IntToStr(self.Age)+'!!!');
  TWomen(Sender).greeting();
  WriteLn('-----------------------------');
  WriteLn(LineEnding);
end;

{ TJustTest }

procedure TJustTest.fire;
var
  Person,another : TPerson;
  women : TWomen;
begin
  Person := TPerson.Create();
  Person.Name := 'Jhon';Person.Age:=24;
  another := TPerson.Create();
  another.Name := 'Doe';another.Age:=42;

  women := TWomen.Create();
  GEventDispatcher.addEvent('test',@Person.sayHello);
  GEventDispatcher.addEvent('second',@another.sayHello);
  try
    WriteLn(GEventDispatcher.all.Text);

    GEventDispatcher.dispatch('test',women);
    GEventDispatcher.dispatch('second',women);
  finally
    women.Free;
    Person.Free;
    another.Free;
    WriteLn('all stuff freed..');
  end;
end;

var
  test : TJustTest;
  currentThread: TThread;
begin
  GEventDispatcher := TEventDispatcher.Create;
  test := TJustTest.Create;
  try
    WriteLn('Im First!!!');
    //test.fire();
    currentThread := TThread.CurrentThread;
    TThread.Queue(currentThread,@test.fire);
    WriteLn('Size Of TThread Instance',TThread.CurrentThread.InstanceSize);
    WriteLn('Im last!!');
  finally
    test.Free;
    GEventDispatcher.Free;
    currentThread.Free;
  end;
end.


