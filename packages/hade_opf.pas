{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hade_opf;

interface

uses
  hdsqldbbase, hdconnectionintf, hdconnection, hdbroker, hdmapper, 
  hdopfmanager, hdquery, hdquerybuilder, hdquerybuilderintf, hdqueryintf, 
  hdsqldbquery, hddatabase, hdimplementorintf, hdsqldbimplementor, 
  hdqueryobjectfactoryintf, hdobjectfactory, hdimplementor, hdcriteria, 
  hdobject, hdpersistentintf, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('hade_opf', @Register);
end.
