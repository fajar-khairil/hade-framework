unit hade.EventDispatcher;

{******************************************************************************
 * This File is Part of HadeFramework Project
 *
 * Copyright (C) Fajar Khairil
 * License MPL 1.1
 *
 * Description : EventDispatcher Implementation
 ******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  hade.eventdispatcherintf,
  contnrs;
Type
  PMethod = ^TMethod;
  { EEventDispatcher }

  EEventDispatcher = class(Exception)end;

  { TEventDispatcher }

  TEventDispatcher = class(IEventDispatcher)
  protected
    FMap: TFPHashList;
  public
    //Dispatch specific event
    procedure dispatch(const AEventName : string;AEvent:Pointer);

    {**
     *
     * Add Event to queue
     * @param AEventName name of the event
     * @param AnotifyEvent procedure to dispatch
     * @param ADispatchOrder order to dispatch event, default is depend on order in queue
    **}
    procedure addEvent(const AEventName : string;
      AEvent : TNotifyEventDispatcher);

    //remove specific event
    procedure removeEvent(const AEventName:string);

    //get all events
    function all:TStringList;

    //clear all events
    procedure clear;

    constructor Create;
    destructor Destroy;override;
  end;

implementation

{ TEventDispatcher }

procedure TEventDispatcher.dispatch(const AEventName: string;  AEvent: Pointer);
var
  lEventList :TFPList;
  iloop: Integer;
  lEvent: PNotifyEventDispatcher;
begin
  lEventList := TFPList(FMap.Find(AEventName));
  if not Assigned(lEventList) then
    raise EEventDispatcher.Create(format('%s not registered on Dispatcher.',[AEventName]));

  for iloop:= 0 to pred(lEventList.Count) do
  begin
    lEvent := PNotifyEventDispatcher(lEventList.Items[iloop]);
    if Assigned(lEvent) then
      lEvent^(AEvent);
  end;
end;

procedure TEventDispatcher.addEvent(const AEventName: string;
  AEvent: TNotifyEventDispatcher);
var
  lEventList: TFPList;
  h : PMethod;
begin
  lEventList := TFPList(FMap.Find(AEventName));
  if not Assigned(lEventList) then
  begin
    lEventList := TFPList.Create();
    FMap.Add(AEventName,lEventList);
  end;

  h := new(PMethod);
  h^.Code:= TMethod(AEvent).Code;
  h^.Data:= TMethod(AEvent).Data;

  lEventList.Add(h);
end;

procedure TEventDispatcher.removeEvent(const AEventName: string);
begin
  FMap.Delete( FMap.FindIndexOf(AEventName) );
end;

function TEventDispatcher.all: TStringList;
var
  iloop: Integer;
begin
  Result :=TStringList.Create;
  try
    for iloop:=0 to pred(FMap.Count) do
    begin
      Result.Add( FMap.NameOfIndex(iloop) );
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TEventDispatcher.clear;
var
  iloop: Integer;
  lCount: Integer;
  jloop: Integer;
  mapCount: Integer;
  local: TFPList;
begin
  mapCount := FMap.Count;
  for iloop:=0 to pred(mapCount) do
  begin
    local := TFPList(FMap.Items[iloop]);
    lCount := local.Count;
    for jloop := 0 to pred( lCount ) do
    begin
      dispose(PMethod(local.Items[jloop]));
    end;
    local.Free;
  end;
  FMap.Pack;
end;

constructor TEventDispatcher.Create;
begin
  FMap := TFPHashList.Create();
end;

destructor TEventDispatcher.Destroy;
begin
  self.Clear;
  FMap.Free;
  inherited Destroy;
end;

end.

