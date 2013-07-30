unit hdimplementor;

{$mode objfpc}{$H+}

interface

uses
  hdbase,
  hdimplementorintf,
  hdmapper,
  hdconnection,
  hdpersistentintf,
  hdobject,
  hdcriteria,
  hdquerybuilderintf;
CONST
  AP_SKIP_ON_ERROR = 0;
  AP_STOP_ON_ERROR = 1;
Type
  { THadeImplementor }

  THadeImplementor = class(THadeBaseObject)
  protected
    FConnection:THadeConnection;

    function GetImplementor(AObject:THadeObjectClass):THadeCustomImplementor;
    function GetImplementor(AClassName:string):THadeCustomImplementor;

    procedure InternalRead(AImplementor:IHadeImplementor;
      ACriteria:THadeCustomQueryBuilder);
  public
    procedure Save(AObject:THadeObject);
    procedure Read(AObject:THadeObject;AFetchMode: TFetchMode);

    procedure Read(AObject:THadeObjectList;ACriteria:THadeCriteria;AFetchMode: TFetchMode);
    procedure ApplyUpdate(AObject:THadeObjectList);

    //procedure InsertRow(const AClassName:string;ACriteria:THadeCriteria);
    //procedure UpdateRow(const AClassName:string;ACriteria:THadeCriteria);
    //procedure DeleteRow(const AClassName:string;ACriteria:THadeCriteria);

    constructor Create(AConnection:THadeConnection);
    destructor Destroy;override;
  end;

implementation
uses
  hdopfmanager;
  //hdquerybuilder;
{ THadeImplementor }

function THadeImplementor.GetImplementor(AObject: THadeObjectClass
  ): THadeCustomImplementor;
begin
  Result := Self.GetImplementor(AObject.ClassName);
end;

function THadeImplementor.GetImplementor(AClassName: string
  ): THadeCustomImplementor;
var
  implClass: THadeImplementorClass;
begin
  Result:= nil;
  implClass:= GHadeOPFManager.ImplementorManager.GetByClassName(AClassName);
  if ImplClass = nil then
    Result:= GHadeOPFManager.ImplementorManager.GetDefaultImplementorFor(FConnection.Broker).Create(FConnection)
  else
    Result:= implClass.Create(FConnection);

  if not Assigned(Result) then
    raise EHadeImplentorException.Create('Class not registered or Default implementor not found.');
end;

procedure THadeImplementor.InternalRead(AImplementor: IHadeImplementor;
  ACriteria: THadeCustomQueryBuilder);
begin
  if Assigned(ACriteria) then
    AImplementor.Query.SQL.Add(ACriteria.GetClause);

  AImplementor.Query.Open;
end;

procedure THadeImplementor.Save(AObject: THadeObject);
var
  Implementor: IHadeImplementor;
  criteria: THadeCustomCriteria;
  pk: THadePropertiesMapper;
begin
  Implementor := Self.GetImplementor( THadeObjectClass(AObject.ClassType) );
  IHadeObject(AObject).MarkDirty;

  case AObject.ObjectState of
    posCreate: Implementor.Insert(AObject);
    posUpdate:
    begin
      Implementor.Update(AObject);
      criteria:= THadeCustomCriteria.create;
      pk := GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName).getPK;
      try
        criteria.Equal(pk.ColumnName,Implementor.ObjectFactory.ObjectToRowString(AObject,pk.PropertyName));
        Implementor.Query.SQL.Add(criteria.GetClause);
        Implementor.Query.ExecSQL;
      finally
        Criteria.Free;
      end;
    end;
    posDelete:
    begin
      Implementor.Delete(AObject);
      criteria:= THadeCustomCriteria.create;
      pk := GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName).getPK;
      try
        criteria.Equal(pk.ColumnName,Implementor.ObjectFactory.ObjectToRowString(AObject,pk.PropertyName));
        Implementor.Query.SQL.Add(criteria.GetClause);
        Implementor.Query.ExecSQL;
      finally
        criteria.Free;
      end;
    end;
  end;

  IHadeObject(AObject).MarkClean;
end;

procedure THadeImplementor.Read(AObject: THadeObject;AFetchMode: TFetchMode);
var
  Implementor: IHadeImplementor;
  tmpCriteria: THadeCustomCriteria;
  pk: THadePropertiesMapper;
  AClassMap: THadeClassMapper;
begin
  (AObject as IHadeObject).MarkDirty;

  Implementor := Self.GetImplementor( THadeObjectClass(AObject.ClassType) );
  Implementor.Select(AObject.ClassType,AFetchMode);

  tmpCriteria := THadeCustomCriteria.Create;
  try
    AClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName);
    pk := AClassMap.getPK;

    tmpCriteria.Equal(AClassMap.Table+'.'+PK.ColumnName,Implementor.ObjectFactory.ObjectToRowString(AObject,pk.PropertyName));

    Self.InternalRead(Implementor,tmpCriteria);
    Implementor.ObjectFactory.RowToObject( AObject,AFetchMode );
  finally
    tmpCriteria.Free;
  end;
end;

procedure THadeImplementor.Read(AObject: THadeObjectList;
  ACriteria: THadeCriteria; AFetchMode: TFetchMode);
var
  Implementor: IHadeImplementor;
begin
  Implementor := Self.GetImplementor(AObject.ChildClass);
  Implementor.Select(AObject.ChildClass,AFetchMode);

  Self.InternalRead(Implementor,ACriteria);

  implementor.ObjectFactory.RowToObjectList(AObject,AFetchMode);
end;

procedure THadeImplementor.ApplyUpdate(AObject: THadeObjectList);
var
  iloop: Integer;
begin
  for iloop:= 0 to pred(AObject.Count)do
    Self.Save(AObject.Items[iloop]);
end;

{procedure THadeImplementor.InsertRow(const AClassName: string;
  ACriteria: THadeCriteria);
var
  Implementor: IHadeImplementor;
  InsertBuilder: THadeQueryBuilderInsert;
  ATable: String;
  AClassMap: THadeClassMapper;
  cols: TStringList;
begin
  Implementor := Self.GetImplementor( AClassName );
  InsertBuilder:= THadeQueryBuilderInsert.Create;
  AClassMap := GHadeOPFManager.PersistenceMapper.FindClassMap(AClassName);
  ATable := AClassMap.Table;
  cols := AClassMap.getWriteableProperties.getColumnLists;

  try
    InsertBuilder.Insert(ATable,cols.CommaText);
  finally
    InsertBuilder.Free;
  end;
end;}

constructor THadeImplementor.Create(AConnection: THadeConnection);
begin
  FConnection:= AConnection;
end;

destructor THadeImplementor.Destroy;
begin
  inherited Destroy;
end;

end.

