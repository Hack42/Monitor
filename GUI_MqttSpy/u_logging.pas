unit u_logging;

// last Modified 2021-07-20

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils;

procedure LogHeader;
procedure AddToDataLog(LogString: shortstring);
procedure LogEvent(EventString: shortstring);
procedure ClearEventLog;
procedure NewLogFileName;

implementation

uses u_main, u_timestrings;

var DataLogFileName, EventLogFilename: string;

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
    Lines.add(MyTime_hmsms + ': ' + EventString);
    SelStart:= length(Text); // autoscroll
  end;       
end;

procedure ClearEventLog;
begin
  if FileExists(EventLogFileName) then DeleteFile(EventLogFileName);
end;

procedure AddToDataLog(LogString: shortstring);
var
  LogFile: text;
begin
  assign(LogFile, DataLogFileName);
  if FileExists(DataLogFileName) then append(LogFile) else rewrite (LogFile);
  writeln(LogFile, DateTimeStamp + LogString);
  close(LogFile);
end;

procedure LogHeader;
var LogStr: shortstring;
begin
  LogStr:= '**  ';
  AddToDataLog(LogStr);
end;

procedure NewLogFileName;
var LogFileName: string;
begin
  LogFileName:= DateTimeStamp2;
  FrmMain.TbxLogFileName.text:= LogFileName;
  EventLogFileName:= LogFileName + '.event';
end;

end.

