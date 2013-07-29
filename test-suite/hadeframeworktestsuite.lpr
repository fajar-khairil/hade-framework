program hadeframeworktestsuite;

{$mode objfpc}{$H+}

uses
  {$IFDEF LINUX}{$IFDEF UseCthreads}
   //cmem,
   cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  GUITestRunner,
  testhadeobject,
  testdatabase,
  testhadequery,
  //testquerybuilder,
  //testconnection,
  testmapper,
  testinit,
  testcustomcriteria,
  testcustomselect,
  testcustominsert,
  testcustomupdate,
  testcustomdelete,
  hdopfmanager, testcriteria, testconnection;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$IFDEF UseCthreads}
    WriteLn('UseCthreads!');
  {$ENDIF}
  Application.Run;
end.

