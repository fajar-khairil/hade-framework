unit hade.DiContainer;

{******************************************************************************
 * This File is Part of HadeFramework Project
 *
 * Copyright (C) Fajar Khairil
 * License MPL 1.1
 *
 * Description : Dependecy Injection Container
 ******************************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs;
Type
    TProcedurePointer = Procedure(out APointer:Pointer) of object;
    TContentType = (ctSingleton,ctFactory);

    { TContentContainer }
    PContentContainer = ^TContentContainer;
    TContentContainer = Record
      Builder: TProcedurePointer;
      Name: string;
      ContentType : TContentType;
    end;

    { EDIContainer }

    EDIContainer = class(Exception);

    { TDIContainer }

    TDIContainer = class
    protected
      FMap : TFPHashList;
      FKeys : TStringList;

      procedure internalBind(
        const AIdentifierName : string;
        AImplementation : TProcedurePointer;
        AContentType:TContentType);
    public
      procedure Singleton(
        const AIdentifierName : string;
        AImplementation : TProcedurePointer);

      procedure Bind(const AIdentifierName : string;
        AImplementation : TProcedurePointer);

      function make(const AIdentifierName:String):pointer;

      procedure Clear;
      function all : TStringList;

      Constructor Create;
      Destructor Destroy;override;
    end;

implementation
uses
  strutils;
{ TContentContainer }

procedure TDIContainer.internalBind(const AIdentifierName: string;
  AImplementation: TProcedurePointer;AContentType:TContentType);
var
  lContent : PContentContainer;
begin
  lContent := new(PContentContainer);
  lContent^.Name:= AIdentifierName;
  lContent^.Builder:= AImplementation;
  lContent^.ContentType:= AContentType;
  FMap.Add(AIdentifierName,lContent);
end;

procedure TDIContainer.Singleton(const AIdentifierName: string;
  AImplementation: TProcedurePointer);
begin
  Self.internalBind(AIdentifierName,AImplementation,ctSingleton);
end;

procedure TDIContainer.Bind(const AIdentifierName: string;
  AImplementation: TProcedurePointer);
begin
  Self.internalBind(AIdentifierName,AImplementation,ctFactory);
end;

function TDIContainer.make(const AIdentifierName: String): pointer;
var
  lfunc: PContentContainer;
begin
  lfunc := PContentContainer(FMap.Find(AIdentifierName));
  if not Assigned(lFunc)  then
    Raise EDIContainer.Create(AIdentifierName+' doesnt exists.');

  if lFunc^.ContentType = ctSingleton then
  begin
    Result := FMap.Find('instance_'+AIdentifierName);
    if not Assigned(Result) then
    begin
      lfunc^.Builder(Result);
      FMap.Add('instance_'+AIdentifierName,Result);
    end;
  end else
  begin
    lfunc^.Builder(Result);
  end;
end;

procedure TDIContainer.Clear;
var
  iloop: Integer;
  obj: Pointer;
begin
  for iloop := 0 to pred(FMap.Count) do
  begin
    obj := FMap.Items[iloop];
    if StrUtils.AnsiStartsStr('instance_' , trimLeft(FMap.NameOfIndex(iloop)) ) then
      TObject(obj).Free
    else
      Dispose(PContentContainer(obj));
  end;
  FKeys.Clear;
end;

function TDIContainer.all: TStringList;
begin
  Result := FKeys;
end;

constructor TDIContainer.Create;
begin
  FMap := TFPHashList.Create;
  FKeys := TStringList.Create;
end;

destructor TDIContainer.Destroy;
begin
  self.Clear;
  FMap.Free;
  FKeys.Free;
  inherited Destroy;
end;

end.

