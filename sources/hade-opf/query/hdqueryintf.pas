unit hdqueryintf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DB,
  sqldb,
  fpjson;

type
  { IHadeQuery }
  IHadeQuery = interface
    ['{8BCF4B4F-D805-48B8-BE2A-FF49E47A8D45}']
    function GetBof: boolean;
    function GetEof: boolean;
    function getFields: TFields;
    function getHandle: pointer;
    function getParams: TParams;
    procedure SetSQL(AValue: TStringlist);
    function GetSQL: TStringlist;

    property Fields:TFields read getFields;
    property Params:TParams read getParams;
    property SQL: TStringlist read GetSQL write SetSQL;
    property Handle: pointer read GetHandle;

    procedure Prepare;
    procedure UnPrepare;
    function RowsAffected: TRowsCount;
    procedure Next;
    procedure prior;
    procedure First;
    procedure last;
    procedure Open;
    procedure Close;
    procedure ExecSQL;

    property EOF:boolean read GetEof;
    property BOF:boolean read GetBof;

    procedure AsJson(out AJsonArray:TJSONArray);
    function ToJsonString:string;
    procedure ExecuteScriptFile(AFileName:string;ATerminator:char = ';');
  end;

implementation

{ THadeCustomQuery }

end.
