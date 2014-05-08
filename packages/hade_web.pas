{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hade_web;

interface

uses
  hdfcgiproccess, hdfcgi, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('hade_web', @Register);
end.
