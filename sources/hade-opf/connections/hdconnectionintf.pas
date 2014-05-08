unit hdconnectionintf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

Type

  { IHadeConnection }

  IHadeDatabaseConnection = Interface
  ['{6C4F96CD-9927-4A5E-9C4D-8ABEBB0A5AF5}']
    procedure Connect();
    procedure Disconnect();
    procedure ExecuteDirect(const ASql:string);
    function GetParams: TStrings;
    procedure StartTransaction();
    procedure Commit();
    procedure Rollback();
    function getHandle: Pointer;

    function getDatabase: string;
    function getHost: string;
    function getPassword: string;
    function getUsername: string;
    procedure setDatabase(AValue: string);
    procedure setHost(AValue: string);
    procedure setPassword(AValue: string);
    procedure setUsername(AValue: string);

    property Params : TStrings read GetParams;
  end;

implementation

end.

