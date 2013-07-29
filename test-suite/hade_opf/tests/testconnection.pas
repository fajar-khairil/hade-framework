unit testconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  //testutils,
  testregistry,
  hdconnection;

type

  { TTestConnection }

  TTestConnection= class(TTestCase)
  protected
    con : THadeConnection;
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

implementation
uses
  testinit;

procedure TTestConnection.SetUp;
begin
  initMapper;
  con := THadeConnection.Create('sqlite');
end;

procedure TTestConnection.TearDown;
begin
  clearMapper;
  con.Free;
end;

initialization

  RegisterTest(TTestConnection);
end.

