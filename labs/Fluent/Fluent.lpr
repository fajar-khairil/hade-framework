program Fluent;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  hade.Fluent
  { you can add units after this };

var
  obj : TFluent;
  lDate: TDateTime;
begin
  lDate := sysutils.StrToDateTime('1990-03-02 11:12:03',FluentFormatSetting);
  obj := TFluent.Create( lDate );
  try
    WriteLn(obj.AsString);
    WriteLn(obj.AsDate);
    WriteLn(obj.AsDateTime);
    WriteLn(obj.AsTime);
    WriteLn(obj.AsDouble);
    WriteLn(obj.AsInteger);
    WriteLn(obj.AsUInteger);
    WriteLn(obj.Value);
  finally
    obj.Free;
  end;
end.

