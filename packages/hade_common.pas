{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hade_common;

interface

uses
  hdutils, hdmapbase, hdrtti, hdbase, hdguid, StringBuilderUnit, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('hade_common', @Register);
end.
