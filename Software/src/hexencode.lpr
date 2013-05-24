program hexencode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,FileUtil
  { you can add units after this }
  , uEncrypt;

type

  { THexEncode }

  THexEncode = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ THexEncode }

procedure THexEncode.DoRun;
var
  fin: TextFile;
  fout: TextFile;
  tmp,tmp1 : Ansistring;
  i: Integer;
  Info: TSearchRec;
const
  my_key = 928371;
begin
  // quick check parameters
  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;
  If FindFirst (AppendPathDelim(UTF8ToSys(GetOptionValue('i','input')))+'*.hex',faAnyFile,Info)=0 then
    Repeat
      AssignFile(fin,AppendPathDelim(UTF8ToSys(GetOptionValue('i','input')))+Info.Name);
      Reset(fin);
      AssignFile(fout,AppendPathDelim(UTF8ToSys(GetOptionValue('o','output')))+Info.Name);
      Rewrite(fout);
      while not EOF(fin) do
        begin
          readln(fin,tmp);
          setlength(tmp1,length(tmp));
          i := length(tmp);
          tmp1 := Encrypt(tmp,my_key); // verschlüsseln
          tmp := Decrypt(tmp1,my_key);
          writeln(fout,tmp1);
        end;
      CloseFile(fin);
      CloseFile(fout);
    Until FindNext(info)<>0;
  FindClose(Info);

  // stop program loop
  Terminate;
end;

constructor THexEncode.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor THexEncode.Destroy;
begin
  inherited Destroy;
end;

procedure THexEncode.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: THexEncode;

begin
  Application:=THexEncode.Create(nil);
  Application.Run;
  Application.Free;
end.

