unit hdobjectfactory;
{$mode objfpc}{$H+}

interface

uses
  hdqueryobjectfactoryintf,
  hdquery,
  Classes,
  db,
  hdpersistentintf,
  hdobject;

type
  { THadeObjectFactory }

  THadeObjectFactory = class(THadeBaseQueryObjectFactory)
  protected
    fQuery: THadeQuery;
  public
    procedure FieldToProp(AObject:TObject;AProp,AField:string;AFetchMode: TFetchMode);override;

    procedure RowToObject(AObject: TObject; AFetchMode: TFetchMode); override;
    procedure ObjectToRow(AObject: TObject); override;
    procedure RowToObjectList(AObject: TObject;AFetchMode:TFetchMode); override;

    constructor Create(AQuery: THadeQuery);
  end;

implementation

uses
  //hdutils,
  hdrtti,
  typinfo,
  hdmapper,
  hdopfmanager;

{ THadeObjectFactory }

procedure THadeObjectFactory.FieldToProp(AObject: TObject; AProp,
  AField: string; AFetchMode: TFetchMode);
var
  FK: THadePropertiesMapper;
  FKClass: TClass;
  cobj: THadeObject;
  RMap: TRelationMap;
  Field: TField;
  FMap: THadeClassMapperList;
begin
  Field:= FQuery.Fields.FindField(AField);
  if not Assigned(Field) then Exit;
  FMap:= GHadeOPFManager.PersistenceMapper.ClassLists;
  case typinfo.PropType(AObject,AProp) of
    tkBool,tkEnumeration: hdrtti.setOrdinalProp(AObject,AProp,Field.AsInteger);
    tkFloat:
    begin
      if typinfo.GetPropInfo(AObject,AProp)^.PropType^.Name = 'TDateTime' then
        hdrtti.setFloatProp(AObject,AProp,Field.AsDateTime)
      else
        hdrtti.setFloatProp(AObject,AProp,Field.AsFloat);
    end;
    tkClass:
    begin
      RMap := GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName).RelationList.Find(AProp);
      if not Assigned(RMap) then exit;//just exit if relation not found

      case RMap.RelationType of
        rtOneToOne ://One-To-One Relationship
        begin
          FKClass:= hdRtti.getHdObjectClassProp(AObject,AProp);
          if not Assigned( fMap.Find(FKClass.ClassName) ) then exit;//exit if class not registered

          cobj:= THadeObjectClass(FKClass).Create(THadeCustomObject(AObject));
          FK:= fMap.Find(FKClass.ClassName).Find(RMap.RelationProperty);
          if not Assigned(FK) then exit;

          if AFetchMode = fcJoin then
            Self.RowToObject(cobj,AFetchMode)
          else
            Self.FieldToProp(cobj,FK.PropertyName,FK.ColumnName,AFetchMode);//do recursive

          IHadeObject(cobj).SetState(posPK);//set the state
          hdrtti.sethdObjectProp(AObject,AProp,cObj);//add to the parent
        end;
      end;

    end;
    else begin
      //string type
      if hdrtti.IsStringProp(AObject,AProp) then
        hdrtti.setStringProp(AObject,AProp,Field.AsString)
      else if hdrtti.IsIntegerProp(AObject,AProp) then
        hdrtti.setIntegerProp(AObject,AProp,Field.AsInteger)
      else Self.RaiseError('Unknown or Unsuported Property Type');
    end;
  end;
end;

procedure THadeObjectFactory.RowToObject(AObject: TObject;AFetchMode:TFetchMode);
var
  iloop: Integer;
  ClassMap: THadeClassMapper;
  Props: TStringList;
  prop: THadePropertiesMapper;
begin
  inherited RowToObject(AObject,AFetchMode);

  ClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName);
  Props:= ClassMap.getPropertiesList;
  try
    for iloop:=0 to pred(Props.Count) do
    begin
      prop:= ClassMap.Find(Props[iloop]);
      if AFetchMode = fcJoin then
        Self.FieldToProp(AObject,Prop.PropertyName,
                          ClassMap.Table+'_'+Prop.ColumnName,AFetchMode)
      else
        Self.FieldToProp(AObject,Prop.PropertyName,Prop.ColumnName,AFetchMode);
    end;
  finally
    Props.Free;
  end;

  IHadeObject( THadeObject(AObject) ).MarkClean;
end;

procedure THadeObjectFactory.RowToObjectList(AObject: TObject;AFetchMode:TFetchMode);
var
  cObj: THadeObject;
begin
  inherited RowToObjectList(AObject,AFetchMode);

  While not FQuery.EOF do
  begin
    cObj:= THadeObjectList(AObject).ChildClass.Create(THadeObjectList(AObject));
    Self.RowToObject(cobj,AFetchMode);

    THadeObjectList(AObject).Add(cObj);
    FQuery.Next;
  end;
end;

procedure THadeObjectFactory.ObjectToRow(AObject: TObject);
begin
  inherited ObjectToRow(AObject);
end;

constructor THadeObjectFactory.Create(AQuery: THadeQuery);
begin
  fQuery := AQuery;
end;

end.
