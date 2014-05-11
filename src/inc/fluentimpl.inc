{ TCustomFluent }

function TCustomFluent.getCount: ptrUint;
begin
  Result := FList.Count;
end;

function TCustomFluent.GetCurrent: T;
begin
  FList.Find(FKeyList[FCursor],Result);
end;

procedure TCustomFluent.clear;
begin
  FList.Iterate(nil,@Iterate_FreeObjects);
  FKeyList.Clear;
  Reset;
end;

function TCustomFluent.First: T;
begin
  FCursor := 0;
  FList.find(FKeyList[FCursor],Result);
end;

function TCustomFluent.Last: T;
begin
  FCursor := pred(FList.Count);
  FList.find(FKeyList[FCursor],Result);
end;

function TCustomFluent.Extract(const ItemKey: shortstring): T;
var
  idx: Integer;
begin
  FList.Find(ItemKey,Result);
  FList.Remove(ItemKey);
  idx := FKeyList.IndexOf(ItemKey);
  FKeyList.Delete( idx );

  if FCursor >= idx then
    FCursor := pred(idx);
end;

procedure TCustomFluent.Remove(const ItemKey: shortstring);
var
  idx: Integer;
begin
  FList.Remove(ItemKey);
  idx := FKeyList.IndexOf(ItemKey);
  FKeyList.Delete( idx );

  if FCursor >= idx then
    FCursor := pred(idx);
end;

function TCustomFluent.MoveNext: Boolean;
begin
  Result := FCursor <> pred(FList.Count);
  if Result then
    inc(FCursor);
end;

procedure TCustomFluent.Reset;
begin
  FCursor := 0;
end;

function TCustomFluent.Items(const AKeyName: shortstring): T;
begin
  if FList.Find(AKeyName,Result) then
  begin
    FKeyList.Add(AKeyName);
    exit;
  end else
  begin
    Result := T.Create(variants.Null);
    FList.Add(AKeyName,Result);exit;
  end;
end;

constructor TCustomFluent.Create;
begin
  FList:=TStringHashMap.Create(2047,False);
  FKeyList := TStringList.Create;
  self.Reset;
end;

destructor TCustomFluent.Destroy;
begin
  self.Clear;
  FList.Free;
  FKeyList.Free;
  inherited Destroy;
end;

{ TCustomFluent }
