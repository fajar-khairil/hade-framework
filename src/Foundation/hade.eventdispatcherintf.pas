unit hade.eventdispatcherintf;
{*****************************Version: MPL 1.1*********************************
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *******************************************************************************
 * This File is Part of HadeFramework Project
 * Copyright (C) Fajar Khairil
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

