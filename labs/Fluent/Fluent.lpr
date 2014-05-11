program Fluent;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  hade.Fluent,
  hade.Utils
  { you can add units after this };

var
  obj : TFluent;
  lDate: TDateTime;
begin
  lDate := sysutils.StrToDateTime('1990-03-02 11:12:03',HadeDefaultFormatSettings);
  obj := TFluent.Create;
  obj.Items('birth').AsDateTime := lDate;
  obj.Items('name').AsString := 'Fajar';
  obj.Items('age').AsInteger := 24;
  try
    WriteLn('Property Count : ',obj.Count);
    WriteLn('---------------------------');
    WriteLn(obj.Items('birth').AsString);
    WriteLn(obj.Items('name').AsString);
    WriteLn(obj.Items('age').AsString);

    obj.Remove('birth');
    WriteLn('Property Count after removing birth key : ',obj.Count);

    obj.Items('just').AsString:= 'Jhon';
    WriteLn('New property added just : ',obj.Items('just').AsString);
    WriteLn('Property Count after adding just key : ',obj.Count);

    obj.clear();
    WriteLn('Property Count after clear : ',obj.Count);

  finally
    //fluentObj.Free;
    obj.Free;
  end;
end.

