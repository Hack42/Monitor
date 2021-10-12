unit u_main;

{$mode objfpc}{$H+}
// Collapse all procedures:  alt shift 1

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type { TFrmMain }

  TFrmMain = class(TForm)
    ButtConnect: TButton;
    ButtDisconnect: TButton;
    ButtNewFilename: TButton;
    ButtSetBrokerIP: TButton;
    CbxRelay1: TCheckBox;
    CbxRelay2: TCheckBox;
    CbxRelay3: TCheckBox;
    CbxRelay4: TCheckBox;
    CbxRelay5: TCheckBox;
    CbxRelay6: TCheckBox;
    CbxRelay7: TCheckBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Request2: TRadioGroup;
    RbGreen2: TRadioButton;
    RbGreenBlink2: TRadioButton;
    RbOFF2: TRadioButton;
    RbRed1: TRadioButton;
    RbGreenBlink1: TRadioButton;
    RbGreen1: TRadioButton;
    RbOFF1: TRadioButton;
    Request1: TRadioGroup;
    RbRed2: TRadioButton;
    TbxMonitorInstance: TEdit;
    TbxP2p5: TEdit;
    TbxHeartBeatReceived: TEdit;
    TbxStatus: TEdit;
    TbxVersionString: TEdit;
    TbxVOC: TEdit;
    TbxRelays: TEdit;
    TbxPhase_L2: TEdit;
    TbxPhase_L3: TEdit;
    TbxPhase_L4: TEdit;
    TbxPhase_L5: TEdit;
    TbxPhase_L6: TEdit;
    TbxPhase_L7: TEdit;
    TbxCurrent_L4: TEdit;
    TbxCurrent_L5: TEdit;
    TbxCurrent_L6: TEdit;
    TbxCurrent_L7: TEdit;
    TbxPhase_L1: TEdit;
    TbxCO2: TEdit;
    TbxCurrent_L1: TEdit;
    TbxCurrent_L2: TEdit;
    TbxCurrent_L3: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    TbxLogFileName: TEdit;
    Memo: TMemo;
    TbxP10: TEdit;
    TbxTemperature: TEdit;
    TbxHygro: TEdit;
    TbxBaro: TEdit;
    TbxDallasTemperatures_0: TEdit;
    TbxDallasTemperatures_1: TEdit;
    TbxDallasTemperatures_2: TEdit;
    TbxDallasTemperatures_3: TEdit;
    TbxDallasTemperatures_4: TEdit;
    TbxDallasTemperatures_5: TEdit;
    TbxDallasTemperatures_6: TEdit;
    TbxDallasTemperatures_7: TEdit;
    TbxDallasTemperatures_8: TEdit;
    TbxDallasTemperatures_9: TEdit;
    TbxTimeOut: TEdit;
    TbxBrokerIP: TEdit;
    TbxHeartBeatSent: TEdit;
    Timer10Hz: TTimer;
    procedure ButtConnectClick(Sender: TObject);
    procedure ButtDisconnectClick(Sender: TObject);
    procedure ButtNewFilenameClick(Sender: TObject);
    procedure ButtSetBrokerIPClick(Sender: TObject);
    procedure CbxRelayChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RbGreen1Change(Sender: TObject);
    procedure RbGreen2Change(Sender: TObject);
    procedure RbGreenBlink1Change(Sender: TObject);
    procedure RbGreenBlink2Change(Sender: TObject);
    procedure RbOFF1Change(Sender: TObject);
    procedure RbOFF2Change(Sender: TObject);
    procedure RbRed1Change(Sender: TObject);
    procedure RbRed2Change(Sender: TObject);
    procedure Timer10HzTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmMain: TFrmMain;
  TargetPath: string;

implementation
{$R *.lfm}

uses u_client, u_logging;

const
  VersionString = ' v 2021-10-03';
  SettingsFileName = 'MQTTBrokerIP.txt';

procedure TFrmMain.ButtConnectClick(Sender: TObject);
begin
  InitTopics(TbxMonitorInstance.text);
  ButtConnectPressed:= true;
  LogEvent('Main: FrmMain.ButtConnect Pressed');
  Timer10Hz.Enabled:= true;
end;

procedure TFrmMain.ButtDisconnectClick(Sender: TObject);
begin
  ButtDisconnectPressed:= true;
  LogEvent('Main: FrmMain.ButtDisConnect Pressed');
end;

procedure TFrmMain.Timer10HzTimer(Sender: TObject); // called at 10 Hz
begin
  UpdateClient;
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

procedure TFrmMain.ButtNewFilenameClick(Sender: TObject);
begin
  NewLogFileName;
end;

procedure TFrmMain.ButtSetBrokerIPClick(Sender: TObject);
begin
  BrokerIP:= TbxBrokerIP.text;
  WriteSettings;
end;

procedure TFrmMain.CbxRelayChange(Sender: TObject);
begin
  Relays:= 0;
  if CbxRelay1.checked then Relays += $02;
  if CbxRelay2.checked then Relays += $04;
  if CbxRelay3.checked then Relays += $08;
  if CbxRelay4.checked then Relays += $10;
  if CbxRelay5.checked then Relays += $20;
  if CbxRelay6.checked then Relays += $40;
  if CbxRelay7.checked then Relays += $80;
  TbxRelays.text:= format('%3d', [Relays]);
  DoPublish_SetRelays:= true;
end;

procedure TFrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteSettings;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  NewLogFileName;
  ReadSettings;
  Caption:= ClientID + VersionString;
  LogEvent('Main: FrmMain.Create');
end;

procedure EvaluateRequestLights1;
begin
  RequestLights:= RequestLights and $F0;
  with FrmMain do
  begin
    if RbRed1.checked then RequestLights += $01;
    if RbGreenBlink1.checked then RequestLights += $02;
    if RbGreen1.checked then RequestLights += $04;
    RequestLights += $08; // cancel switch
    TbxRelays.text:= format('%2x', [RequestLights]);
  end;
  DoPublish_SetRequestLights:= true;
end;

procedure EvaluateRequestLights2;
begin
  RequestLights:= RequestLights and $0F;
  with FrmMain do
  begin
    if RbRed2.checked then RequestLights += $10;
    if RbGreenBlink2.checked then RequestLights += $20;
    if RbGreen2.checked then RequestLights += $40;
    RequestLights += $80; // cancel switch
    TbxRelays.text:= format('%2x', [RequestLights]);
  end;
  DoPublish_SetRequestLights:= true;
end;

procedure TFrmMain.RbRed1Change(Sender: TObject);
begin
  EvaluateRequestLights1;
end;

procedure TFrmMain.RbRed2Change(Sender: TObject);
begin
  EvaluateRequestLights2;
end;

procedure TFrmMain.RbGreen1Change(Sender: TObject);
begin
  EvaluateRequestLights1;
end;

procedure TFrmMain.RbGreen2Change(Sender: TObject);
begin
  EvaluateRequestLights2;
end;

procedure TFrmMain.RbGreenBlink1Change(Sender: TObject);
begin
  EvaluateRequestLights1;
end;

procedure TFrmMain.RbGreenBlink2Change(Sender: TObject);
begin
  EvaluateRequestLights2;
end;

procedure TFrmMain.RbOFF1Change(Sender: TObject);
begin
  EvaluateRequestLights1;
end;

procedure TFrmMain.RbOFF2Change(Sender: TObject);
begin
  EvaluateRequestLights2;
end;

end.

