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
  obj := TFluent.Create;
  try
    obj.Items('test').AsDateTime := lDate;
    obj.Items('name').AsString := 'Fajar';

    WriteLn(obj.Items('test').AsString);
    WriteLn(obj.Items('test').AsDate);
    WriteLn(obj.Items('test').AsDateTime);
    WriteLn(obj.Items('test').AsTime);
    WriteLn(obj.Items('test').AsDouble);
    WriteLn(obj.Items('test').AsInteger);
    WriteLn(obj.Items('test').AsUInteger);
    WriteLn(obj.Items('test').Value);

    WriteLn(obj.Items('name').AsString);

    WriteLn(obj.Count);
  finally
    //fluentObj.Free;
    obj.Free;
  end;
end.

