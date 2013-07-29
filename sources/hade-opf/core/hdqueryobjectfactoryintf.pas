unit hdqueryobjectfactoryintf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
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
    procedure FieldToProp(AObject:TObject;AProp,AField:string; AFetchMode: TFetchMode);virtual;abstract;
    function ObjectToRowString(AObj:TObject;APropName:string):string;
  end;

implementation
uses
  hdobject,
  hdopfmanager,
  hdmapper,
  typinfo,
  hdrtti,
  hdutils;

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

function THadeBaseQueryObjectFactory.ObjectToRowString(AObj: TObject;
  APropName: string): string;
var
  RMap: TRelationMap;
  rObj: THadeObject;
  FK: THadePropertiesMapper;
  FMapper: THadeClassMapperList;
begin
  FMapper:= GHadeOPFManager.PersistenceMapper.ClassLists;
  case PropType(AObj,APropName) of
    tkBool,tkEnumeration: Result:= IntToStr(hdrtti.getOrdinalProp(AObj,APropName));
    tkClass:
    begin
      RMap:= FMapper.Find(AObj.ClassName).RelationList.Find(APropName);
      rObj:= THadeObject( hdrtti.getHdObjectProp(AObj,APropName) );
      if not assigned(rObj) then begin Result:= 'NULL' ;exit;end;
      case RMap.RelationType of
        rtOneToOne:
        begin
          FK:= FMapper.Find(rObj.ClassName).Find(RMap.RelationProperty);
          if not Assigned(Fk) then exit;
          Result:= Self.ObjectToRowString(rObj,FK.PropertyName);
        end;
      end;
    end;
    else begin
      if hdrtti.IsStringProp(AObj,APropName) then
        Result := QuotedStr(hdrtti.getStringProp(AObj,APropName))
      else if hdrtti.IsIntegerProp(AObj,APropName) then
        Result := IntToStr(hdrtti.getIntegerProp(AObj,ApropName))
      else if typinfo.PropType(AObj,APropName) = tkFloat then
      begin
        if GetPropInfo(AObj,APropName)^.PropType^.Name = 'TDateTime' then
          Result := QuotedStr(DateTimeToStr( getFloatProp(AObj,ApropName),hdutils.HadeDBFormatSettings))
        else
          Result := FloatToStr( getFloatProp(AObj,ApropName) );
      end else Self.RaiseError(APropName+' Unsuported Type');
    end;
  end;
end;

end.

