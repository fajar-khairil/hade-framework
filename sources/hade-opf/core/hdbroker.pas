unit hdbroker;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;
Type
  THadeBroker = (
    UNKNOWN_BROKER,//reserved for checking
    SqlDBFirebird,
    SqlDBMySQL,
    SqlDBSqLite,
    SqlDBPostgree,
    SqlDBOracle
  );

  function BrokerAsString(ABroker:THadeBroker):string;
  function StringAsBroker(Astr:String):THadeBroker;
  function isSQLDb(ABroker:THadeBroker):Boolean;

implementation

function BrokerAsString(ABroker: THadeBroker): string;
begin
  case ABroker of
    SqlDBFirebird: Result:= 'SqlDBFirebird';
    SqlDBMySQL: Result:= 'SqlDBMySQL';
    SqlDBSqLite: Result:= 'SqlDBSqLite';
    SqlDBPostgree: Result:= 'SqlDBPostgree';
    SqlDBOracle: Result:= 'SqlDBOracle';
  end;
end;

function StringAsBroker(Astr: String): THadeBroker;
begin
  case LowerCase(Astr) of
    'sqldbfirebird': Result:= SqlDBFirebird;
    'sqldbmysql': Result:= SqlDBMySQL;
    'sqldbsqlite': Result:= SqlDBSqLite;
    'sqldbpostgree': Result:= SqlDBPostgree;
    'sqldboracle': Result:= SqlDBOracle;
    else Result:= UNKNOWN_BROKER;
  end;
end;

function isSQLDb(ABroker: THadeBroker): Boolean;
begin
  Result := ABroker in [SqlDBFirebird,SqlDbMySQL,SqlDbSqlite,SqlDbPostgree,SqlDbOracle];
end;

end.

