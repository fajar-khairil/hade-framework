unit hdfcgi;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdfcgiproccess,
  custweb;

type
  { THadeFCGIApplication }

  THadeFCGIApplication = class(TCustomWebApplication)
  private
    function GetAddress: string;
    function GetLingerTimeOut: integer;
    function getMaxThreadCount: integer;
    function GetOnUnknownRecord: TUnknownRecordEvent;
    function GetPort: integer;
    function getTimeout: integer;
    procedure SetAddress(AValue: string);
    procedure SetLingerTimeOut(AValue: integer);
    procedure setMaxThreadCount(AValue: integer);
    procedure SetOnUnknownRecord(AValue: TUnknownRecordEvent);
    procedure SetPort(AValue: integer);
    procedure setTimeout(AValue: integer);
  protected
    function InitializeWebHandler: TWebHandler; override;
  public
    property Port: integer read GetPort write SetPort;
    property LingerTimeOut : integer read GetLingerTimeOut write SetLingerTimeOut;
    property Address: string read GetAddress write SetAddress;
    Property OnUnknownRecord : TUnknownRecordEvent Read GetOnUnknownRecord Write SetOnUnknownRecord;
    property Timeout:integer read getTimeout write setTimeout;
    property MaxThreadCount:integer read getMaxThreadCount write setMaxThreadCount;
  end;

var
  Application: THadeFCGIApplication = nil;
  ShowCleanUpErrors: boolean = False;

implementation

uses
  CustApp;

procedure InitFCGI;
begin
  Application := THadeFCGIApplication.Create(nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

procedure DoneFCGI;
begin
  try
    if CustomApplication = Application then
      CustomApplication := nil;
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      raise;
  end;
end;

{ THadeFCGIApplication }

function THadeFCGIApplication.GetAddress: string;
begin
  Result:= THadeFCGIHandler(self.WebHandler).Address;
end;

function THadeFCGIApplication.GetLingerTimeOut: integer;
begin
  Result := THadeFCGIHandler(self.WebHandler).LingerTimeOut;
end;

function THadeFCGIApplication.getMaxThreadCount: integer;
begin
  Result := THadeFCGIHandler(self.WebHandler).MaxThreadCount;
end;

function THadeFCGIApplication.GetOnUnknownRecord: TUnknownRecordEvent;
begin
  Result:= THadeFCGIHandler(self.WebHandler).OnUnknownRecord;
end;

function THadeFCGIApplication.GetPort: integer;
begin
  Result:= THadeFCGIHandler(self.WebHandler).Port;
end;

function THadeFCGIApplication.getTimeout: integer;
begin
  Result:= THadeFCGIHandler(WebHandler).TimeOut;
end;

procedure THadeFCGIApplication.SetAddress(AValue: string);
begin
  THadeFCGIHandler(self.WebHandler).Address:= AValue;
end;

procedure THadeFCGIApplication.SetLingerTimeOut(AValue: integer);
begin
  THadeFCGIHandler(self.WebHandler).LingerTimeOut:=AValue;
end;

procedure THadeFCGIApplication.setMaxThreadCount(AValue: integer);
begin
  THadeFCGIHandler(self.WebHandler).MaxThreadCount:= AValue;
end;

procedure THadeFCGIApplication.SetOnUnknownRecord(AValue: TUnknownRecordEvent);
begin
 THadeFCGIHandler(self.WebHandler).OnUnknownRecord:= AValue;
end;

procedure THadeFCGIApplication.SetPort(AValue: integer);
begin
 THadeFCGIHandler(self.WebHandler).Port:=AValue;
end;

procedure THadeFCGIApplication.setTimeout(AValue: integer);
begin
 THadeFCGIHandler(self.WebHandler).TimeOut :=AValue;
end;

function THadeFCGIApplication.InitializeWebHandler: TWebHandler;
begin
  Result:= THadeFCGIHandler.Create(self);
end;

initialization
  InitFCGI;

finalization
  DoneFCGI;
end.
