unit hdqueryobjectfactoryintf;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  hdbase,
  hdpersistentintf;
Type
  EHadeQueryObjectFactoryException = class(EHadeException);

  { IHadeQueryObjectFactoryIntf }

  IHadeQueryObjectFactoryIntf = interface
  ['{05F764FD-F452-401C-8C31-AE67597DC9F7}']
    procedure RowToObject(AObject:TObject; AFetchMode: TFetchMode);
    procedure ObjectToRow(AObject:TObject);
    procedure RowToObjectList(AObject:TObject;AFetchMode:TFetchMode);
    procedure FieldToProp(AObject:TObject;AProp,AField:string; AFetchMode: TFetchMode);
    function ObjectToRowString(AObj:TObject;APropName:string):string;
  end;

  { THadeBaseQueryObjectFactory }

  THadeBaseQueryObjectFactory = class(TInterfacedObject,IHadeQueryObjectFactoryIntf)
  protected
    procedure RaiseError(const AMsg:string);
  public
    procedure RowToObject(AObject:TObject; AFetchMode: TFetchMode);virtual;
    procedure ObjectToRow(AObject:TObject);virtual;
    procedure RowToObjectList(AObject:TObject;AFetchMode:TFetchMode);virtual;
    procedure FieldToProp(AObject:TObject;AProp,AField:string;
      AFetchMode: TFetchMode);virtual;
    function ObjectToRowString(AObj:TObject;APropName:string):string;virtual;
  end;

implementation
uses
  hdobject;

{ THadeBaseQueryObjectFactory }

procedure THadeBaseQueryObjectFactory.RaiseError(const AMsg: string);
begin
  raise EHadeQueryObjectFactoryException.Create(AMsg);
end;

procedure THadeBaseQueryObjectFactory.RowToObject(AObject: TObject; AFetchMode: TFetchMode);
begin
  if not sysutils.Supports(AObject,IHadeObject) then
    RaiseError('Not Supported interface.');
end;

procedure THadeBaseQueryObjectFactory.ObjectToRow(AObject: TObject);
begin
  if not sysutils.Supports(AObject,IHadeObject) then
    RaiseError('Not Supported interface.');
end;

procedure THadeBaseQueryObjectFactory.RowToObjectList(AObject: TObject;
  AFetchMode: TFetchMode);
begin
 if not sysutils.Supports(AObject,IHadeObjectList) then
    RaiseError('Not Supported interface.');
end;

procedure THadeBaseQueryObjectFactory.FieldToProp(AObject: TObject; AProp,
  AField: string; AFetchMode: TFetchMode);
begin
 if not sysutils.Supports(AObject,IHadeObject) then
   RaiseError('Not Supported interface.');
end;

function THadeBaseQueryObjectFactory.ObjectToRowString(AObj: TObject;
  APropName: string): string;
begin
 if not sysutils.Supports(AObj,IHadeObject) then
   RaiseError('Not Supported interface.');
end;

end.

