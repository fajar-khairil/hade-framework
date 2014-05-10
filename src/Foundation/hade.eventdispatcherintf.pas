unit hade.EventDispatcherIntf;

{******************************************************************************
 * This File is Part of HadeFramework Project
 *
 * Copyright (C) Fajar Khairil
 * License MPL 1.1
 *
 * Description : EventDispatcher Interface
 ******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  Classes;
Type
  PNotifyEventDispatcher = ^TNotifyEventDispatcher;
  TNotifyEventDispatcher = Procedure (Sender : Pointer) of Object;

  { IEventDispatcher }
  {$INTERFACES CORBA}
  IEventDispatcher  = Interface
  ['{6BBA9613-8354-4B40-9EF7-1CC2AB48D72A}']
  //Dispatch specific event
  procedure dispatch(const AEventName : string;AEvent:Pointer);

  {**
   *
   * Add Event to queue
   * @param AEventName name of the event
   * @param AEvent procedure to dispatch
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
  end;

implementation

end.

