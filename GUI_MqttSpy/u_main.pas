unit u_main;

{$mode objfpc}{$H+}
// Collapse all procedures:  alt shift 1

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls;

type { TFrmMain }

  TFrmMain = class(TForm)
    ButtDisconnect: TButton;
    ButtConnect: TButton;
    ButtSetBrokerIP: TButton;
    ButtNewLogFile: TButton;
    TbxLogFileName: TEdit;
    Memo: TMemo;
    TbxBrokerIP: TEdit;
    Timer10Hz: TTimer;
    procedure ButtDisconnectClick(Sender: TObject);
    procedure ButtConnectClick(Sender: TObject);
    procedure ButtNewLogFileClick(Sender: TObject);
    procedure ButtSetBrokerIPClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer10HzTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  VersionString = 'MQTT_Spy v 2021-10-03';
  ClientID = 'MQTT_Spy';
  Topic_All = '#';

var
  FrmMain: TFrmMain;
  BrokerIP :shortstring;

procedure DoSubscriptons;
procedure WriteSettings;

implementation
{$R *.lfm}

uses u_client, u_logging;

const SettingsFileName = 'MQTTBrokerIP.txt';

procedure DoSubscriptons;
begin
  Client.Subscribe(Topic_All);
end;

procedure TFrmMain.ButtConnectClick(Sender: TObject);
begin
  LogEvent('ButtConnect Pressed');
  mq_DoUpdateClient:= true;
  mq_TerminateSession:= false;
  mq_ConnectClient(BrokerIP, ClientID);
 end;

procedure TFrmMain.ButtDisconnectClick(Sender: TObject);
begin
  LogEvent('ButtDisconnect Pressed');
  mq_TerminateSession:= true;
end;

procedure TFrmMain.ButtSetBrokerIPClick(Sender: TObject);
begin
  BrokerIP:= TbxBrokerIP.text;
  WriteSettings;
end;

procedure TFrmMain.ButtNewLogFileClick(Sender: TObject);
begin
  NewLogFileName;
end;

procedure TFrmMain.Timer10HzTimer(Sender: TObject); // called at 10 Hz
const Counter: integer = 0;
begin
  inc(counter);
  if Counter > 300 then // send a dummy publication each 30 seconds
  begin
    Send:= true;
    Counter:= 0;
  end;
  mq_UpdateClient;
end;

procedure ReadSettings;
var SettingsFile: text;
begin
  if not FileExists(SettingsFileName) then
  begin
    LogEvent ('Main: Read Settings File: File not found: ' + SettingsFileName);
    exit;
  end;
  LogEvent ('Main: Read Settings File: ' + SettingsFileName);
  assign (SettingsFile, SettingsFileName);
  reset (SettingsFile);
  readln (SettingsFile, BrokerIP);
  FrmMain.TbxBrokerIP.text:= BrokerIP;
  close(SettingsFile);
  LogEvent ('Main: MQTT BrokerIP= ' + BrokerIP);
end;

procedure WriteSettings;
var SettingsFile: text;
begin
  assign (SettingsFile, SettingsFileName);
  rewrite (SettingsFile);
  writeln (SettingsFile, BrokerIP);
  close(SettingsFile);
end;

procedure TFrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteSettings;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  caption:= VersionString;
  NewLogFileName;
  ReadSettings;
  Memo.Clear;
  LogEvent('FrmMain.Create');
end;


end.

