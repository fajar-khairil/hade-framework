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
    TProcedurePointer = Procedure(out APointer:Pointer);
    TContentType = (ctSingleton,ctFactory);
    { TContentContainer }

    TContentContainer = class
    private
      FBuilder: TProcedurePointer;
      FName: string;
      FContentType : TContentType;
    public
      property IdentfierName : string read FName write FName;
      property Builder : TProcedurePointer read FBuilder write FBuilder;
      property ContentType : TContentType read FContentType write FContentType;

      Constructor Create(const AIdentifierName :string;ABuilder : TProcedurePointer;AContentType:TContentType);
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

{ TContentContainer }

constructor TContentContainer.Create(const AIdentifierName: string;
  ABuilder: TProcedurePointer; AContentType: TContentType);
begin
  FBuilder := ABuilder;
  FName := AIdentifierName;
  FContentType := AContentType;
end;

{ TDIContainer }

procedure TDIContainer.internalBind(const AIdentifierName: string;
  AImplementation: TProcedurePointer;AContentType:TContentType);
begin
    FMap.Add(AIdentifierName,TContentContainer.Create(AIdentifierName,AImplementation,AContentType));
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
  lfunc: TContentContainer;
begin
  lfunc := TContentContainer(FMap.Find(AIdentifierName));
  if not Assigned(lFunc)  then
    Raise EDIContainer.Create(AIdentifierName+' doesnt exists.');

  if lFunc.ContentType = ctSingleton then
  begin
    Result := FMap.Find('instance_'+AIdentifierName);
    if not Assigned(Result) then
    begin
      lfunc.Builder(Result);
      FMap.Add('instance_'+AIdentifierName,Result);
    end;
  end else
  begin
    lfunc.Builder(Result);
  end;
end;

procedure TDIContainer.Clear;
var
  iloop: Integer;
  obj: TObject;
begin
  for iloop := 0 to pred(FMap.Count) do
  begin
    obj := TObject(FMap.Items[iloop]);
    obj.Free;
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

