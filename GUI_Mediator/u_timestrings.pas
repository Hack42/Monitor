unit u_timestrings;

{$mode objfpc}{$H+}

interface

uses SysUtils;

function DateTimeStamp: string;
function DateTimeStampMS: string;
function DateTimeStamp2: string;
function MyDate: string;
function MyDate_DMD_EN: string;
function MyDate_DMD_NL: string;
function MyTime_hm: string;
function MyTime2_hm: string;
function MyTime_hms: string;
function HourPassed: boolean;
function MinutePassed: boolean;
function DayPassed: boolean;
function ByteToTime(ByteVal: byte): string; // byte with 10 minute intervals to hh:mm
function Seconds2HHMMSS(Seconds: longint): string;

implementation

function DayPassed: boolean;
var Y, M, D: word;
const PrevD: integer= -1;
begin
  DayPassed:= false;
  decodedate(now, Y, M, D);
  if D <> PrevD then
  begin
    DayPassed:= true;
    Prevd:= D;
  end;
end;

function HourPassed: boolean;
var h, m, s, ms: word;
const Prevh: integer= -1;
begin
  HourPassed:= false;
  decodetime(now, h, m, s, ms);
  if h <> Prevh then
  begin
    HourPassed:= true;
    Prevh:= h;
  end;
end;

function FiveMinutesPassed: boolean;
const FirstTime: boolean = true;
var h, m, s, ms: word;
begin
	decodetime(now, h, m, s, ms);
  FiveMinutesPassed := false;
	if(m mod 2) = 0 then
  begin
	if FirstTime then // give true only the first time it happens.
    begin
      FiveMinutesPassed:= true;
    	FirstTime:= false;
    end;
  end
  else
    FirstTime:= true;
end;

function MinutePassed: boolean;
var h, m, s, ms: word;
const Prevm: integer= -1;
begin
  MinutePassed:= false;
  decodetime(now, h, m, s, ms);
  if m <> Prevm then
  begin
    MinutePassed:= true;
    Prevm:= m;
  end;
end;

function MyTime_hm: string;
var h, m, s, ms: word;
T1, T2: string;
begin
	  decodetime(now, h, m, s, ms);
	  str(h:2, T1); if h<10 then T1[1]:= '0';
	  str(m:2, T2); if m<10 then T2[1]:= '0';
	  MyTime_hm:= T1 + ':' + T2;
end;

// MyTime2 has no : so can be used as filename
function MyTime2_hm: string;
var h, m, s, ms: word;
T1, T2: string;
begin
	  decodetime(now, h, m, s, ms);
	  str(h:2, T1); if h<10 then T1[1]:= '0';
	  str(m:2, T2); if m<10 then T2[1]:= '0';
	  MyTime2_hm:= T1 + T2;
end;

// MyTime_hms has seconds
function MyTime_hms: string;
var h, m, s, ms: word;
T1, T2, T3: string;
begin
decodetime(now, h, m, s, ms);
	  str(h:2, T1); if h<10 then T1[1]:= '0';
	  str(m:2, T2); if m<10 then T2[1]:= '0';
	  str(s:2, T3); if s<10 then T3[1]:= '0';
	  MyTime_hms:= T1 + ':' + T2 + ':' + T3;
end;

// MyTime_hmsms has seconds and milliseconds
function MyTime_hmsms: string;
var h, m, s, ms: word;
T1, T2, T3, T4: string;
begin
	  decodetime(now, h, m, s, ms);
	  str(h:2, T1); if h<10 then T1[1]:= '0';
	  str(m:2, T2); if m<10 then T2[1]:= '0';
	  str(s:2, T3); if s<10 then T3[1]:= '0';
	  str(ms:3, T4);
	  MyTime_hmsms:= T1 + ':' + T2 + ':' + T3 + '~' + T4;
end;

function DateTimeStamp: string;
begin
DateTimeStamp:= MyDate + '_' + MyTime_hms;
end;

function DateTimeStampMS: string;
begin
DateTimeStampMS:= MyDate + '_' + MyTime_hmsms;
end;

// DateTimeStamp2 has no : so can be used as filename
function DateTimeStamp2: string;
begin
  DateTimeStamp2:= MyDate + '_' + MyTime2_hm;
end;

function MyDate: string;
var Y, M, d: word;
T1, T2, T3: string;
begin
	 decodedate(now, Y, m, d);
	 Y:= Y mod 100;
	 str(Y:2, T1); if Y<10 then T1[1]:= '0';
	 str(M:2, T2); if M<10 then T2[1]:= '0';
	 str(d:2, T3); if d<10 then T3[1]:= '0';
	 MyDate:= T1 + '-' + T2 + '-' + T3;
end;

function MyDate_DMD_NL: string;
const Days: array[1..7] of string = ('Zo', 'Ma', 'Di', 'Wo', 'Do', 'Vr', 'Za');
var Y, M, d: word;
T1, T2, Dow: string;
begin
	 decodedate(now, Y, m, d);
	 Dow:= days[DayOfWeek(now)];
	 str(M:2, T1); if M<10 then T1[1]:= '0';
	 str(d:2, T2); if d<10 then T2[1]:= '0';
	 MyDate_DMD_NL:= Dow + ' ' + T1 + '-' + T2;
end;

function MyDate_DMD_EN: string;
const Days: array[1..7] of string = ('Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa');
var Y, M, d: word;
T1, T2, Dow: string;
begin
	 decodedate(now, Y, m, d);
	 Dow:= days[DayOfWeek(now)];
	 str(M:2, T1); if M<10 then T1[1]:= '0';
	 str(d:2, T2); if d<10 then T2[1]:= '0';
	 MyDate_DMD_EN:= Dow + ' ' + T1 + '-' + T2;
end;

function ByteToTime(ByteVal: byte): string;
var h, m: integer;
T1, T2: string;
begin
  m:= (ByteVal mod 6)*10;
  h:= ByteVal div 6;
  str(h:2, T1); if h<10 then T1[1]:= '0';
  str(m:2, T2); if m<10 then T2[1]:= '0';
  ByteToTime:= T1 + ':' + T2;
end;

function Seconds2HHMMSS(Seconds: longint): string;
var h, m, s: integer;
T1, T2, T3: string;
begin
  s:= Seconds mod 60;
  Seconds -= s;
  Seconds:= Seconds div 60;
  m:= Seconds mod 60;
  Seconds -= m;
  h:= Seconds div 60;
  str(s:2, T3); if s<10 then T3[1]:= '0';
  str(m:2, T2); if m<10 then T2[1]:= '0';
  str(h:2, T1); if h<10 then T1[1]:= '0';
  Seconds2HHMMSS:= T1 + ':' + T2 + ':' + T3;
end;

end.

