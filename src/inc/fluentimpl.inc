{ TCustomFluent }

function TCustomFluent.getCount: ptrUint;
begin
  Result := FList.Count;
end;

procedure TCustomFluent.clear;
begin
  FList.Iterate(nil,@Iterate_FreeObjects);
  FKeyList.Clear;
end;

procedure TCustomFluent.Remove(const ItemKey: shortstring);
var
  idx: Integer;
  item: PData;
begin
  item := FList.Remove(ItemKey);
  if Assigned(item) then
  begin
    T(item).Free;
    idx := FKeyList.IndexOf(ItemKey);
    FKeyList.Delete( idx );
  end;
end;

function TCustomFluent.Items(const AKeyName: shortstring): T;
begin
  if FList.Find(AKeyName,Result) then
  begin
    FKeyList.Add(AKeyName);
    exit;
  end else
  begin
    Result := T.Create;
    FList.Add(AKeyName,Result);exit;
  end;
end;

constructor TCustomFluent.Create;
begin
  FList:=TStringHashMap.Create(2047,False);
  FKeyList := TStringList.Create;
end;

destructor TCustomFluent.Destroy;
begin
  self.Clear;
  FList.Free;
  FKeyList.Free;
  inherited Destroy;
end;

{ TCustomFluent }
