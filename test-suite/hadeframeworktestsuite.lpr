program hadeframeworktestsuite;

{$mode objfpc}{$H+}

uses
  {$IFDEF LINUX}
   //cmem,
   cthreads,
  {$ENDIF}
  Interfaces,
  sysutils,
  Forms,
  GUITestRunner,
  testhadeobject,
  testdatabase,
  testhadequery,
  //testconnection,
  testmapper,
  testinit,
  testcustomcriteria,
  testcustomselect,
  testcustominsert,
  testcustomupdate,
  testcustomdelete,
  hdopfmanager,
  testcriteria;
Type

{ TSimpleExceptionHandler }

TSimpleExceptionHandler = class
public
  Procedure doOnException(Sender : TObject; E : Exception);
end;

{ TSimpleExceptionHandler }

procedure TSimpleExceptionHandler.doOnException(Sender: TObject; E: Exception);
begin
  WriteLn(E.Message);
end;

var
  EHandler: TSimpleExceptionHandler;
begin
  EHandler:= TSimpleExceptionHandler.Create;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.OnException:= @EHandler.doOnException;
  Application.Run;
  EHandler.Free;
end.

