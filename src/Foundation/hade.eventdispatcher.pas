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
  StrHashMap,
  contnrs;
Type
  {$i type.inc}
  { EEventDispatcher }

  EEventDispatcher = class(Exception)end;

  { TEventDispatcher }

  TEventDispatcher = class(IEventDispatcher)
  protected
    FMap: TStringHashMap;
    FMapIndex : TFPObjectList;
    FKeys : TStringList;
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
    procedure Clear;

    constructor Create(const ACaseSensitive:boolean = True);
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
  if not FMap.Find(AEventName,lEventList) then
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
  if not FMap.Find(AEventName,lEventList) then
  begin
    lEventList := TFPList.Create();
    FMap.Add(AEventName,lEventList);
    FMapIndex.Add(lEventList);
    FKeys.Add(AEventName);
  end;

  h := new(PMethod);
  h^.Code:= TMethod(AEvent).Code;
  h^.Data:= TMethod(AEvent).Data;

  lEventList.Add( h );
end;

procedure TEventDispatcher.removeEvent(const AEventName: string);
begin
  Fmap.Remove(AEventName);
end;

function TEventDispatcher.all: TStringList;
begin
  Result := FKeys;
end;

procedure TEventDispatcher.Clear;
var
  iloop: Integer;
  lMap: TFPList;
  jloop: Integer;
begin
  for iloop := 0 to pred( FMapIndex.Count ) do
  begin
    lMap := TFPList(FMapIndex.Items[iloop]);
    for jloop:=0 to pred( lMap.count ) do
    begin
      Dispose(PMethod(lMap.Items[jloop]));
    end;
    lMap.Free;
  end;

  FKeys.Clear;
end;

constructor TEventDispatcher.Create(const ACaseSensitive:boolean);
begin
  FMap := TStringHashMap.Create(2047,ACaseSensitive);
  FMapIndex := TFPObjectList.Create(False);
  FKeys := TStringList.Create;
end;

destructor TEventDispatcher.Destroy;
begin
  self.Clear;
  FMapIndex.Free;
  FKeys.Free;
  FMap.Free;
  inherited Destroy;
end;

end.

