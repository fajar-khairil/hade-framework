{ THadeClassMapperList }

function THadeClassMapperList.GetItem(Index: integer): THadeClassMapper;
begin
  Result := THadeClassMapper(inherited GetItem(Index));
end;

procedure THadeClassMapperList.CloneMapClass(ASource, ATarget: THadeClassMapper);
var
  iloop: Integer;
begin
  for iloop:=0 to pred(ASource.Count) do
  begin
    //ATarget.
    ATarget.add(ASource.Items[iloop].PropertyName,
      THadePropertiesMapper.Create(ASource.Items[iloop].PropertyName,
      ASource.Items[iloop].ColumnName,ASource.Items[iloop].ColumnInfo,
      ASource.Items[iloop].DataType),
      False);
  end
end;

function THadeClassMapperList.Find(const s: shortstring): THadeClassMapper;
begin
  Result := THadeClassMapper(inherited Find(s));
  if not Assigned(Result) then
    raise EHadeMapperException.Create('Cannot find '+s);
end;

function THadeClassMapperList.FindTable(const s: shortstring): THadeClassMapper;
var
  iloop: Integer;
begin
  for iloop:=0 to pred(self.Count) do
  begin
    if uppercase(self.Items[iloop].Table) = uppercase(s) then
    begin
      Result:=self.Items[iloop];
      break;
    end;
  end;
end;

function THadeClassMapperList.MapClass(AClass: THadeObjectClass;
  ATableName: string; AExtendClass: THadeObjectClass): THadeClassMapper;
var
  CSource: THadeClassMapper;
  CTarget: THadeClassMapper;
begin
  CTarget := THadeClassMapper.Create(AClass,ATableName);
  if Assigned(AExtendClass) then
  begin
    CSource := Self.Find(AExtendClass.ClassName);
    if Assigned(CSource) then
      self.CloneMapClass(CSource,CTarget);
  end;

  Self.add(AClass.ClassName,CTarget);

  Result := CTarget;
end;

function THadeClassMapperList.add(const AName: shortstring;
  const AObject: THadeClassMapper; const FreeIfExists: boolean): integer;
begin
  Result := inherited add(AName, AObject, FreeIfExists);
end;

function THadeClassMapperList.addReplace(const AName: shortstring;
  const AObject: THadeClassMapper): integer;
begin
  Result := inherited addReplace(AName, AObject);
end;

{ THadeRelationList }

function THadeRelationList.Find(const s: shortstring): TRelationMap;
begin
  Result:= TRelationMap(inherited Find(s));
  if not Assigned(Result) then
    Raise EHadeMapperException.Create('No Relation found for '+s);
end;

procedure THadeRelationList.MapOneToOne(APropertyName:string;ARelationObject:TRelationMap);
begin
  Self.Add(APropertyName,ARelationObject);
end;

procedure THadeRelationList.MapOneToMany(APropertyName: string;ARelationObject:TRelationMap);
begin
  Self.Add(APropertyName,ARelationObject);
end;

procedure THadeRelationList.MapManyToMany(APropertyName: string;
  ARelationObject: TManyToManyMapper);
begin
  Self.Add(APropertyName,ARelationObject);
end;

function THadeRelationList.GetRelationType(ApropertyName: string): TRelationType;
var
  cClassType: TClass;
begin
  if not Assigned(Self.Find(APropertyName)) then
  begin
    Result:= rtOneToOne;
  end else
  begin
    cClassType:= Self.Find(APropertyName).ClassType;
    if cClassType = TRelationMap then
      Result:= TRelationMap(Self.Find(APropertyName)).RelationType
    else if cClassType = TManyToManyMapper then
      Result:= rtManyToMany;
  end;
end;

constructor THadeRelationList.Create(AParentClass: THadeObjectClass);
begin
  inherited Create(True);
  FParentClass:= AParentClass;
end;

{ TRelationMap }

constructor TRelationMap.Create(const APropertyName, ARelationProp: string;
  ARelationType: TRelationType);
begin
  FProperty:= APropertyName;
  FRelationProp:= ARelationProp;
  FRelationType:= ARelationType;
end;

{ THadePropertiesMapper }

constructor THadePropertiesMapper.Create(APropName, AColName: string;
  AcolInfo: TSetPtkInfo; ADataType: ftType);
begin
  fPropName := APropName;
  fColName := AColName;
  fInfo := AcolInfo;
  fType := ADataType;
end;

{ THadeClassMapper }

procedure THadeClassMapper.RaiseError(const AMsg: string);
begin
  raise EHadeMapperException.Create(AMsg);
end;

function THadeClassMapper.getPK: THadePropertiesMapper;
var
  iloop: Integer;
begin
  if Assigned(fPK) then
  begin
    Result:= fPk;
    exit;
  end;

  for iloop:=0 to pred(count) do
  if ptkPk in items[iloop].ColumnInfo then
  begin
    Result := items[iloop];
    fPK := Result;
    break;
  end;

  if not Assigned(Result) then self.RaiseError('no primary key not specified on this mapper');
end;

function THadeClassMapper.add(APropName: string;
  const AObject: THadePropertiesMapper; const FreeIfExists: boolean): integer;
begin
  Result := inherited add(APropName,AObject,FreeIfExists);
end;

function THadeClassMapper.addReplace(const AName: shortstring;
  const AObject: THadePropertiesMapper): integer;
begin
  Result := inherited addReplace(AName,AObject);
end;

function THadeClassMapper.Find(const s: shortstring): THadePropertiesMapper;
begin
  Result := THadePropertiesMapper(inherited Find(s));
end;

function THadeClassMapper.FindByColumn(const s: shortstring
  ): THadePropertiesMapper;
var
  iloop: Integer;
begin
  Result := nil;
  for iloop:=0 to pred(self.Count) do
  begin
    if uppercase(self.items[iloop].ColumnName) = uppercase(s) then
    begin
      Result := items[iloop];
      break;
    end;
  end;
end;

function THadeClassMapper.getPropertiesList: TStringList;
var
  iloop: Integer;
begin
  Result := TStringList.Create;
  for iloop:=0 to pred(self.Count) do
    Result.Add(self.items[iloop].PropertyName);
end;

function THadeClassMapper.getColumnLists: TStringList;
var
  iloop: Integer;
begin
  Result := TStringList.Create;
  for iloop:=0 to pred(self.Count) do
    Result.Add(self.items[iloop].ColumnName);
end;

function THadeClassMapper.getWriteableProperties: THadeClassMapper;
var
  iloop: Integer;
begin
  Result := THadeClassMapper.Create(self._class,self.Table,False);
  for iloop:=0 to pred(count)do
    if not (ptkReadOnly in self.Items[iloop].ColumnInfo) then
      Result.add(self.Items[iloop].PropertyName,self.Items[iloop],False);
end;

function THadeClassMapper.PropsAsCommaText: string;
var
  tmp: String;
  iloop: Integer;
begin
  tmp:='';
  for iloop:=0 to pred(count)do
    tmp := tmp+self.Items[iloop].PropertyName+',';

  Result := hdutils.deleteLastChar(tmp);
end;

function THadeClassMapper.ColumnsAsCommaText: string;
var
  tmp: String;
  iloop: Integer;
begin
  tmp:='';
  for iloop:=0 to pred(count)do
    tmp := tmp+self.Items[iloop].ColumnName+',';

  Result := hdutils.deleteLastChar(tmp);
end;

procedure THadeClassMapper.MapOneToOne(const APropertyName, AFieldName: string;
  ARelationField: string);
begin
  Self.MapProperties(APropertyName,AFieldName,[ptkFK],ftReference);
  FRelationList.MapOneToOne(APropertyName,TRelationMap.Create(APropertyName,ARelationField,rtOneToOne));
end;

procedure THadeClassMapper.MapOneToMany(const APropertyName, ARelationField,
  ASelfRelationField: string);
begin
  FRelationList.MapOneToMany(APropertyName,TRelationMap.Create(ASelfRelationField,ARelationField,rtOneToMany));
end;

procedure THadeClassMapper.MapProperties(APropName, AColName: string;
  APtkInfo: TSetPtkInfo; ADataType: ftType);
begin
  Self.add(APropName, THadePropertiesMapper.Create(APropName, AColName, APtkInfo, ADataType));
end;

function THadeClassMapper.getProperty(APropName: string): THadePropertiesMapper;
begin
  Result := THadePropertiesMapper(inherited Find(APropName));
end;

function THadeClassMapper.GetItem(Index: integer): THadePropertiesMapper;
begin
  Result := THadePropertiesMapper(inherited getItem(Index));
end;

function THadeClassMapper.getClassName: string;
begin
  Result:= FClass.ClassName;
end;

constructor THadeClassMapper.Create(AClass: THadeObjectClass; ATable: string;
  AFreeObject: boolean);
begin
  inherited create(AFreeObject);
  FRelationList:= THadeRelationList.Create(Self._class);
  fClass := AClass;
  fTable := ATable;
end;

destructor THadeClassMapper.Destroy;
begin
  FRelationList.Destroy;
  inherited Destroy;
end;

{ THadeDatabaseMapperList }

function THadeDatabaseMapperList.GetItem(Index: integer): THadeDatabaseMapper;
begin
  Result := THadeDatabaseMapper(inherited getItem(Index));
end;

procedure THadeDatabaseMapperList.RaiseError(const AMsg: string);
begin
  raise EHadeMapperException.Create(AMsg);
end;

function THadeDatabaseMapperList.Find(const s: shortstring): THadeDatabaseMapper;
begin
  Result := nil;//initial value

  if trim(s) = EmptyStr then
    Result := self.getDefault
  else
    Result := THadeDatabaseMapper(inherited Find(s));

  if not Assigned(Result) then
    RaiseError('Connection '+s+' not registered.');
end;

function THadeDatabaseMapperList.add(const AName: shortstring;
  const AObject: THadeDatabaseMapper; const FreeIfExists: boolean): integer;
begin
  if AObject.IsDefault then
  begin
    if not Assigned(self.getDefault) then
      Result := inherited add(AName, AObject, FreeIfExists)
    else
    begin
      AObject.Free;
      RaiseError('Can`t add more default connection.');
    end;
  end
  else
    Result := inherited add(AName, AObject, FreeIfExists);
end;

function THadeDatabaseMapperList.addReplace(const AName: shortstring;
  const AObject: THadeDatabaseMapper): integer;
begin
  if AObject.IsDefault then
  begin
    if not Assigned(self.getDefault) then
      Result := inherited addReplace(AName, AObject)
    else
      RaiseError('Can`t add more default connection.');
  end
  else
    Result := inherited addReplace(AName, AObject);
end;

function THadeDatabaseMapperList.getDefault: THadeDatabaseMapper;
var
  iloop: integer;
begin
  Result := nil;
  for iloop := 0 to pred(Count) do
  begin
    if items[iloop].IsDefault then
    begin
      Result := items[iloop];
      break;
    end;
  end;
end;

{ THadeDatabaseMapper }

function THadeDatabaseMapper.GetParams: TStrings;
begin
  Result:=FParams;
end;

procedure THadeDatabaseMapper.SetParams(AValue: TStrings);
begin
  FParams:=AValue;
end;

constructor THadeDatabaseMapper.Create(AConnectionName: string; AIsDefault: boolean);
begin
  fConnectionName := AConnectionName;
  fDefault := AIsDefault;
  FParams:= TStringList.Create;
end;

destructor THadeDatabaseMapper.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;
