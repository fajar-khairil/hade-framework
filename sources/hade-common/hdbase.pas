unit hdbase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;
CONST
  { MAJOR.MINOR-STATUS[a:ALPHA,b:BETA,rc:Release Candidate,f:FINAL].REVISION_NUMBER }
  VERSION = '0.1-a.1';
  PACKAGENAME = 'Hade-Framework';
Type
  EHadeException = class(Exception)
  end;

  { THadeBaseObject }
  THadeBaseObject = class(TPersistent)
  private
    fTag: ptrUint;
  protected
  public
    property Tag:ptrUint read fTag write fTag;

    class function version:string;
  end;

implementation

{ THadeBaseObject }

class function THadeBaseObject.version: string;
begin
  Result := PACKAGENAME+' '+VERSION;
end;

end.

