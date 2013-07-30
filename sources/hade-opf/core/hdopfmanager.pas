unit hdopfmanager;

{$mode objfpc}{$H+}

interface

uses
  //Classes,
  //SysUtils,
  hdbase,
  hdmapper,
  hdimplementormanager;
Type

  { THadeOPFManager }

  THadeOPFManager = class(THadeBaseObject)
  protected
    FImplManager: THadeImplementorManager;
    FMapper: THadeMapper;
  public
    property ImplementorManager: THadeImplementorManager read FImplManager;
    property PersistenceMapper:THadeMapper read FMapper;
    //Cache
    //Log
    constructor Create;
    destructor Destroy;override;
  end;

var
  GHadeOPFManager : THadeOPFManager = nil;
implementation
{ THadeOPFManager }

constructor THadeOPFManager.Create;
begin
  FMapper := THadeMapper.Create;
  FImplManager:= THadeImplementorManager.Create;
end;

destructor THadeOPFManager.Destroy;
begin
  FMapper.Free;
  FImplManager.Free;
  inherited Destroy;
end;

initialization
  GHadeOPFManager := THadeOPFManager.Create;
Finalization
  GHadeOPFManager.Free;
end.

