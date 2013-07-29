unit mainmodul;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  hdsession;

type

  { TMainModule }

  TMainModule = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure indexRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure longRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    db:THadeSession;
  public
    { public declarations }
  end;

var
  MainModule: TMainModule;

implementation
uses
  hdfcgi,
  testinit;
{$R *.lfm}

{ TMainModule }

procedure TMainModule.DataModuleCreate(Sender: TObject);
begin
  db:=THadeSession.Create();
end;

procedure TMainModule.DataModuleDestroy(Sender: TObject);
begin
  db.Free;
end;

procedure TMainModule.indexRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  person: TPerson;
begin
  person:= TPerson.Create(nil);
  person.personID:= 2;
  db.Read(person,True);
  try
    AResponse.SetCustomHeader('Content-Type','json/application');
    Aresponse.Contents.Add(person.ToJSONString());
  finally
    handled:=True;
    person.Free;
  end;
end;

procedure TMainModule.longRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  sleep(3000);
  AResponse.Contents.Add('Arrrgh sorry i was sleep for 3 second..');
  Handled:=True;
end;

initialization
  RegisterHTTPModule('main', TMainModule);
end.

