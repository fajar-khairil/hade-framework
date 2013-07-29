program testfcgi;

{$mode objfpc}{$H+}

uses
  {$IFDEF LINUX}
  cthreads,
  {$ENDIF}
  Classes,
  sysutils,
  mainmodul,
  hdfcgi,
  httpdefs,
  testinit;

procedure DoOnError(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  AResponse.Contents.Add('Ooopps An Error ocurred..'+AnException.Message);
  Handled:=True;
end;

begin
  Application.OnShowRequestException:= @DoOnError;
  Application.Title:= 'Synapse Fastcgi Application';
  Application.AllowDefaultModule := True;
  Application.DefaultModuleName := 'main';
  Application.Port:= 9090;
  Application.MaxThreadCount:= 2;
  Application.Initialize;
  testinit.initMapper;
  Application.Run;
end.

