{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hadefoundation;

interface

uses
  hade.EventDispatcher, hade.EventDispatcherIntf, hade.DiContainer, 
  hade.CustomApp, hade.Fluent, hade.Utils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('hadefoundation', @Register);
end.
