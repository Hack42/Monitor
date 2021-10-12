unit u_logging;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils;

procedure LogEvent(EventString: shortstring);
procedure NewLogFileName;

implementation

uses u_main, u_timestrings;

var EventLogFilename: string;

procedure LogEvent(EventString: shortstring);
var
EventLogFile: text;
begin
  assign(EventLogFile, EventLogFileName);
  if FileExists(EventLogFileName) then append(EventLogFile) else rewrite (EventLogFile);
  writeln(EventLogFile, DateTimeStampMS + '  ' + EventString);
  close(EventLogFile);
  with FrmMain.Memo do
  begin
    Lines.add(EventString);
    SelStart:= length(Text); // autoscroll
  end;
end;

procedure NewLogFileName;
var LogFileName: string;
begin
  LogFileName:= DateTimeStamp2;
  FrmMain.TbxLogFileName.text:= LogFileName;
  EventLogFileName:= LogFileName + '.event';
end;

end.

