unit u_numstrings;

{$mode objfpc}{$H+}

interface

uses SysUtils;

function FIval(InpStr: string): longint;
function FRval(InpStr: string): real;
function BytesView(InpStr: string; Nch: integer): string;
function BytesView(InpStr: string): string;
function BytesViewHex(InpStr: string; Nch: integer): string;
function BytesViewHex(InpStr: string): string;
function formatS000p000(value: real): string;
function StripPath(Path: string): string;

implementation

uses strutils;

function FIval(InpStr: string): longint;
var Temp: longint;
begin
  val(InpStr, Temp);
  FIval:= Temp;
end;

function FRval(InpStr: string): real;
var Temp: real;
begin
  val(InpStr, Temp);
  FRval:= Temp;
end;

function BytesView(InpStr: string; Nch: integer): string;
var N, A: integer; Temp, Total: string;
begin
  Total:= '';
  for N:= 1 to Nch do
	  begin
    A:= byte(InpStr[N]);
    str(A:3, Temp);
    if Temp[2]= ' ' then Temp[2]:= '0';
    if Temp[1]= ' ' then Temp[1]:= '0';
    Total:= Total + Temp + ': ';
	  end;
  BytesView:= Total;
end;

function BytesView(InpStr: string): string;
var N, A: integer; Temp, Total: string;
begin
  Total:= '';
  for N:= 1 to length(InpStr) do
	  begin
    A:= byte(InpStr[N]);
    str(A:3, Temp);
    if Temp[2]= ' ' then Temp[2]:= '0';
    if Temp[1]= ' ' then Temp[1]:= '0';
    Total:= Total + Temp + ': ';
	  end;
  BytesView:= Total;
end;

function BytesViewHex(InpStr: string; Nch: integer): string;
var N, A: integer; Temp, Total: string;
begin
  Total:= '';
  for N:= 1 to Nch do
	  begin
    A:= byte(InpStr[N]);
    Temp:= format('%2x',[A]);
    if Temp[1]= ' ' then Temp[1]:= '0';
    Total:= Total + Temp + ':';
	  end;
  BytesViewhex:= Total;
end;

function BytesViewHex(InpStr: string): string;
var N, A: integer; Temp, Total: string;
begin
  Total:= '';
  for N:= 1 to length(InpStr) do
	  begin
    A:= byte(InpStr[N]);
    Temp:= format('%2x',[A]);
    if Temp[1]= ' ' then Temp[1]:= '0';
    Total:= Total + Temp + ':';
	  end;
  BytesViewhex:= Total;
end;

function formatS000p000(Value: real): string;
var
  temp: string;
  negative: boolean;
begin
  negative:= Value < 0.0;
  temp:= format('%8.3f', [abs(Value)]);
  if negative then temp[1]:= '-' else temp[1]:= '+';
  if temp[2] = ' ' then temp[2]:= '0';
  if temp[3] = ' ' then temp[3]:= '0';
  formatS000p000:= temp;
end;

function StripPath(Path: string): string;
var N: Integer;
begin
for N := Length(Path) downto 1 do
  if (Path[N] = '\') or (Path[N] = '/') Then
  begin
    StripPath := Midstr(Path, N + 1, Length(Path));
    Exit;
  end;
If N <= 1 Then StripPath := Path;
end;

end.

