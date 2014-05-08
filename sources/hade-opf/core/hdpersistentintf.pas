unit hdpersistentintf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hdobject;
Type
  TFetchMode = (fcLazy,fcJoin);
  { IHadePersistent }
  {$INTERFACES CORBA}
  IHadePersistent = interface
  ['{30160880-6411-4E6A-8C00-ACF1CC201867}']
    procedure Save(AObject:THadeObject);
    procedure Read(AObject: THadeObject;AFetchMode:TFetchMode = fcLazy);
  end;
implementation

end.

