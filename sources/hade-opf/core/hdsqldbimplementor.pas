unit hdsqldbimplementor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  hdimplementorintf,
  hdquerybuilder,
  hdpersistentintf;

type

  { THadeSQLDBImplementor }

  THadeSQLDBImplementor = class(THadeCustomImplementor)
  protected
    function BuildSelectStatement(AObject:TClass; AFetchMode: TFetchMode):string;
    function BuildInsertStatement(AObject:TObject):string;
    function BuildUpdateStatement(AObject:TObject):string;
    function BuildDeleteStatement(AObject:TObject):string;

    procedure CleanQuery;
  public

    //implement it
    procedure Update(AObject: TObject);override;
    procedure Insert(AObject: TObject);override;
    procedure Delete(AObject: TObject);override;
    procedure Select(AObject: TClass;AFetchMode: TFetchMode);override;
  end;

implementation

uses
  hdmapper,
  hdutils,
  hdrtti,
  hdbroker,
  hdopfmanager;

{ THadeSQLDBImplementor }

function THadeSQLDBImplementor.BuildSelectStatement(AObject: TClass;
  AFetchMode: TFetchMode): string;
var
  FQueryBuilder: THadeQueryBuilderSelect;
  AClassMap: THadeClassMapper;
  PropLists: TStringList;
  iloop: Integer;
  jloop: Integer;
  rClassName: String;
  RMap: THadeClassMapper;
  FKProp: TRelationMap;
  rColumns: TStringList;
  kloop: Integer;
  tmp: String;
begin
  Result:= '';
  tmp:='';
  AClassMap:= GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName);
  FQueryBuilder:= THadeQueryBuilderFactory.GetSelectBuilder(FConnection.Broker);
  FQueryBuilder.From(AClassMap.Table);
  PropLists:= AClassMap.getColumnLists;
  try
    //on AReadRelation = True
    if AFetchMode = fcJoin then
    begin
      for iloop:=0 to pred(PropLists.Count) do
        tmp := tmp+AClassMap.Table+'.'+PropLists[iloop]+' as '+AClassMap.Table+'_'+PropLists[iloop]+',';

      for jloop:=0 to pred(AClassMap.RelationList.Count)do
      begin
        FKProp:= TRelationMap(AClassMap.RelationList.Items[jloop]);
        rClassName:= hdrtti.getHdObjectClassProp(AObject,FKProp.PropertyName).ClassName;
        RMap:= GHadeOPFManager.PersistenceMapper.FindClassMap(rClassName);

        rColumns:= RMap.getColumnLists;
        try
          for kloop:=0 to pred(rColumns.Count)do
            tmp:= tmp+RMap.Table+'.'+rColumns[kloop]+' as '+RMap.Table+'_'+rColumns[kloop]+',';
        finally
          rColumns.Free;
        end;

        tmp:= hdutils.deleteLastChar(tmp);
        FQueryBuilder.Select(tmp);

        FQueryBuilder.Join(RMap.Table,AClassMap.Table+'.'+AClassMap.Find(FKProp.PropertyName).ColumnName,
                    RMap.Table+'.'+RMap.Find(FKProp.RelationProperty).ColumnName);
      end;
    end else
      FQueryBuilder.Select(PropLists.CommaText);

    Result:= FQueryBuilder.GetClause;
  finally
    PropLists.Free;
    FQueryBuilder.Free;
  end;
end;

function THadeSQLDBImplementor.BuildInsertStatement(AObject: TObject): string;
var
  FQueryBuilder: THadeQueryBuilderInsert;
  AClassMap: THadeClassMapper;
  PropLists: THadeClassMapper;
  FValues: String;
  iloop: Integer;
begin
  Result:= '';
  AClassMap:= GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName);
  FQueryBuilder:= THadeQueryBuilderFactory.GetInsertBuilder(FConnection.Broker);
  PropLists:= AClassMap.getWriteAbleProperties;
  try
    FValues:='';
    FQueryBuilder.Insert(AClassMap.Table,proplists.ColumnsAsCommaText);
    for iloop:=0 to pred(PropLists.Count) do
    begin
      FValues:= FValues + FObjectFactory.ObjectToRowString(AObject,PropLists.Items[iloop].PropertyName)+',';
    end;
    FValues:= hdutils.deleteLastChar(FValues);
    FQueryBuilder.Values(FValues);
    Result:= FQueryBuilder.Getclause;
  finally
    FQueryBuilder.Free;
    PropLists.Free;
  end;
end;

function THadeSQLDBImplementor.BuildUpdateStatement(AObject: TObject): string;
var
  FQueryBuilder: THadeQueryBuilderUpdate;
  AClassMap: THadeClassMapper;
  PropLists: THadeClassMapper;
  FValues: String;
  iloop: Integer;
begin
  Result:= '';
  AClassMap:= GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName);
  FQueryBuilder:= THadeQueryBuilderFactory.GetUpdateBuilder(FConnection.Broker);
  PropLists:= AClassMap.getWriteAbleProperties;
  try
    FQueryBuilder.Update(AClassMap.Table,PropLists.ColumnsAsCommaText);

    FValues:= '';
    for iloop:=0 to pred(PropLists.Count)do
    begin
      FValues:= FValues +FObjectFactory.ObjectToRowString(AObject,PropLists.Items[iloop].PropertyName)+',';
    end;
    FValues:= hdutils.deleteLastChar(FValues);
    FQueryBuilder.Values(FValues);
    Result:= FQueryBuilder.GetClause;
  finally
    FQueryBuilder.Free;
    PropLists.Free;
  end;
end;

function THadeSQLDBImplementor.BuildDeleteStatement(AObject: TObject): string;
var
  FQueryBuilder: THadeQueryBuilderDelete;
  AClassMap: THadeClassMapper;
begin
  Result:= '';
  AClassMap:= GHadeOPFManager.PersistenceMapper.FindClassMap(AObject.ClassName);
  FQueryBuilder:= THadeQueryBuilderFactory.GetDeleteBuilder(FConnection.Broker);
  try
    FQueryBuilder.Delete(AClassMap.Table);
    Result:= FQueryBuilder.Getclause;
  finally
    FQueryBuilder.Free;
  end;
end;

procedure THadeSQLDBImplementor.CleanQuery;
begin
  FQuery.Close;
  FQuery.SQL.Clear;
end;

procedure THadeSQLDBImplementor.Update(AObject: TObject);
begin
  inherited Update(AObject);
  FQuery.SQL.Text:= Self.BuildUpdateStatement(AObject);
end;

procedure THadeSQLDBImplementor.Insert(AObject: TObject);
var
  PK: THadePropertiesMapper;
begin
  inherited Insert(AObject);
  FQuery.SQL.Text:= Self.BuildInsertStatement(AObject);
  PK:= GHadeOPFManager.PersistenceMapper.FindClassMap(AOBject.ClassName).getPK;

  if (ptkPK in PK.ColumnInfo) AND (ptkAutoInc in PK.ColumnInfo) then
  begin
    case GHadeOPFManager.PersistenceMapper.Connections.find(FConnection.ConnectionName).Broker of
      SQLDBFirebird:begin
        FQuery.Open;
        FObjectFactory.FieldToProp(AObject,PK.PropertyName,PK.ColumnName,fcLazy);
      end;
      SQLDBMySQL,SQLDBSqlite:begin
        FQuery.ExecSQL;
        hdrtti.setIntegerProp(AObject,PK.PropertyName,FQuery.GetLastInsertID);
      end;
      else Self.RaiseError('Broker not yet implemented.');
    end;
  end else
    FQuery.ExecSQL;
end;

procedure THadeSQLDBImplementor.Delete(AObject: TObject);
begin
  inherited Delete(AObject);
  FQuery.SQL.Text:= Self.BuildDeleteStatement(AObject);
end;

procedure THadeSQLDBImplementor.Select(AObject: TClass; AFetchMode: TFetchMode
  );
begin
  inherited Select(AObject,AFetchMode);
  FQuery.SQL.Text:= Self.BuildSelectStatement(AObject,AFetchMode);
end;

end.
