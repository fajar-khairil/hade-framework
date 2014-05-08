unit hdutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

function deleteFirstChar(var AString: string): string;
function deleteLastChar(var AString: string): string;
function deleteFirstLastChar(var AString: string): string;
function compareStringList(var AExpectedList, AList: TStringList;
  AFreeStringList: boolean = False): boolean;
//convert array of const to TStringList
function constArrayToStringList(AValues: array of const;
  AQuotedStr: boolean = True): TStringList;

var
  HadeDBFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'yyyy-mm-dd';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn:ss';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );
implementation

function deleteLastChar(var AString: string): string;
begin
  System.Delete(AString, Length(AString), 1);
  Result := AString;
end;

function deleteFirstChar(var AString: string): string;
begin
  System.Delete(AString, 1, 1);
  Result := AString;
end;

function deleteFirstLastChar(var AString: string): string;
begin
  Result := deleteFirstChar(Astring);
  Result := deleteLastChar(AString);
end;

function compareStringList(var AExpectedList, AList: TStringList;
  AFreeStringList: boolean): boolean;
var
  iloop: integer;
  idx: integer;
begin
  Result := True;

  try
    for iloop := 0 to pred(AList.Count) do
    begin
      idx := AExpectedList.IndexOf(AList[iloop]);
      if idx < 0 then
      begin
        Result := False;
        break;
      end;

      if AExpectedList[idx] <> AList[iloop] then
      begin
        Result := False;
        break;
      end;
    end;
  finally
    if AFreeStringList then
    begin
      FreeAndNil(AExpectedList);
      FreeAndNil(AList);
    end;
  end;
end;

function constArrayToStringList(AValues: array of const;
  AQuotedStr: boolean): TStringList;
var
  iloop: integer;
begin
  Result := TStringList.Create;
  for iloop := LOW(AValues) to HIGH(AValues) do
  begin
    if AValues[iloop].vtype in [vtString,vtAnsiString,vtWideString,vtWideChar,vtChar] then
      if AQuotedStr then
        Result.Add(QuotedStr( AnsiString(AValues[iloop].VAnsiString) ))
      else
        Result.Add( AnsiString(AValues[iloop].VAnsiString) );

    case AValues[iloop].vtype of
      vtInteger: Result.Add( IntToStr(AValues[iloop].VInteger) );
      vtInt64: Result.Add( IntToStr(AValues[iloop].VInt64^) );
      vtBoolean: Result.Add( BoolToStr(AValues[iloop].VBoolean) );
      vtObject: Result.Add(AValues[iloop].VObject.ClassName);
      vtClass: Result.Add(AValues[iloop].VClass.ClassName);
      vtExtended: Result.Add( FloatToStr(AValues[iloop].VExtended^) );
    end;
  end;
end;

end.
