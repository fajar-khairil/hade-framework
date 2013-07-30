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

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

