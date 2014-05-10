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
  StrHashMap,
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
      FMap : TStringHashMap;
    public
      procedure Singleton(
        const AIdentifierName : string;
        AImplementation : TProcedurePointer);

      procedure Bind(const AIdentifierName : string;
        AImplementation : TProcedurePointer);

      function make(const AIdentifierName:String):pointer;

      //procedure Clear;

      Constructor Create(const ACaseSensitive:boolean = True);
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

procedure TDIContainer.Singleton(const AIdentifierName: string;
  AImplementation: TProcedurePointer);
begin
  FMap.Add(AIdentifierName,TContentContainer.Create(AIdentifierName,AImplementation,ctSingleton));
end;

procedure TDIContainer.Bind(const AIdentifierName: string;
  AImplementation: TProcedurePointer);
begin
  FMap.Add(AIdentifierName,TContentContainer.Create(AIdentifierName,AImplementation,ctFactory));
end;

function TDIContainer.make(const AIdentifierName: String): pointer;
var
  lfunc: TContentContainer;
begin
  lfunc := nil;
  if not FMap.Find(AIdentifierName,lfunc) then
    Result := lFunc;

  if lFunc.ContentType = ctSingleton then
  begin
    if not FMap.Find('instance_'+AIdentifierName,Result) then
    begin
      lfunc.Builder(Result);
      FMap.Add('instance_'+AIdentifierName,Result);
    end;
  end else
  begin
    lfunc.Builder(Result);
  end;
end;

constructor TDIContainer.Create(const ACaseSensitive:boolean);
begin
  FMap := TStringHashMap.Create(2047,ACaseSensitive);
end;

destructor TDIContainer.Destroy;
begin
  FMap.Iterate(TObject.Create,@Iterate_FreeObjects);
  FMap.Free;
  inherited Destroy;
end;

end.

