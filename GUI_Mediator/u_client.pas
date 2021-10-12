unit u_client;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

Const
  ClientID = 'Mediator';

var
  BrokerIP, VersionString: string;
  Topic_SetRelays, Topic_HeartBeat, Topic_Status, Topic_Power, Topic_AirQuality,
     Topic_DallasTemperatures, Topic_SetRequestLights, Topic_HeartBeatAck: ansistring;
  Temperature: real;
  Hygro, Baro, CO2, VOC, P10, P2p5: smallint; // int_16
  DallasTemperatures: array[0..9] of real;
  Status, Relays, RequestLights: byte;
  ButtConnectPressed, ButtDisconnectPressed: boolean;
  DoPublish_SetRelays, DoPublish_SetRequestLights: boolean;
  TimeOut: integer;

procedure UpdateClient;  // called at 10 Hz rate
procedure InitTopics (Instance: string);

implementation

uses u_main, u_logging, u_numstrings, MQTT, strutils, graphics;

type ClientStates =
 (cs_Idle, cs_WaitConnAck, cs_DoSubscription_1,
  cs_WaitSubAck_1, cs_WaitSubAck_2, cs_WaitSubAck_3, cs_WaitSubAck_4,
  cs_WaitSubAck_5, cs_WaitSubAck_6, cs_WaitSubAck_7,
  cs_Running, cs_Failing);

var
  Msg: TMQTTMessage;
  ack: TMQTTMessageAck;
  Client: TMQTTClient;
  Topic, Payload: ansistring;
  SeenConnAck, SeenSubAck, SeenPubAck: boolean;
  ConnectTimeOut: integer;
  ClientState: ClientStates;
  HeartBeats, HeartBeatReceived: byte;
  CH08_Amplitude, CH09_Amplitude, CH10_Amplitude, CH11_Amplitude,
     CH12_Amplitude, CH13_Amplitude, CH14_Amplitude: real;
  CH08_Phase, CH09_Phase, CH10_Phase, CH11_Phase, CH12_Phase, CH13_Phase, CH14_Phase: real;

procedure InitTopics (Instance: string);
begin
  LogEvent ('Initialise Topics for instance: ' + Instance);
  // Topics which this Client publishes:

  Topic_SetRelays:= Instance + '/SetRelays';
  // Payload for relays is an unsigned integer between 0 and 255
  // Relays correspond with bits 1 .. 7.  Bit 0 has no meaning

  // Mediator sends HeartBeats on a regular basis to all PowerMonitors
  // HeartBeats have a payload: a byte value increased after every publish.
  // The Monitors must respond with a HeartBeatAck and the serial number.
  // if the Ack is missing to long Mediator should take actions t.b.d.
  Topic_HeartBeat:= 'Mediator/HeartBeat';
  // Payload: Serial number as 3 digit unsigned byte, increased after each publish.

  // Topics on which this Client has a subscription:

  Topic_Status:= Instance + '/Status';
  // Payload: 3 digit byte value containing status bits
  // String containing Build date and Time of the Firmware

  Topic_Power:= Instance + '/Power';
  // Payload: 7x 6 digit signed integer as amplitude in deciAmperes +
  // 7x int16 as Phase angle in deciDegrees

  Topic_AirQuality:= Instance + '/AirQuality';
  // Payload: Temperature, Hygro, Baro, Co2, Voc, P10, P2p5: 5 digits each.

  Topic_DallasTemperatures:= Instance + '/DallasTemperatures';
  // Payload: 10x 5 digit integer giving Tempertures in deci-degrees Celsius

  Topic_SetRequestLights:= Instance + '/SetRequestLights';
  // Payload:3 digit byte value
  // Bits 0, 1, 2, 3 for RequestBox 1, bits 4, 5, 6, 7 for RequestBox 2.
  // bit 0, 4: red, bit 1, 5: green blinking, bit 2, 6: green.
  // bit 3, 7: cancel switch signal

  Topic_HeartBeatAck:= Instance + '/HeartBeatAck';
  // Payload: Serial number as 3 digit unsigned byte.
  // Should be the same as last published HeartBeat.
end;

procedure Publish (Topic, Payload: ansistring);
begin
  Client.Publish (Topic, Payload);
  LogEvent ('Publish T= ' + Topic  + ' P= ' + Payload);
end;

// Publish Topics and handle Messages from Broker
procedure UpdateClient; // Called at 10Hz by Timer10Hz
const
  PublishState: byte = 0;
  ConnAckTimeOut: byte = 0;
var
  MessageAvailable: boolean;
begin
  case ClientState of
  cs_Idle:
    begin
      if ButtConnectPressed then
      begin
        ButtConnectPressed:= false;
        Client:= TMQTTClient.Create(BrokerIP, 1883);
        Client.clientID:= ClientID;
        Client.Connect;
        LogEvent('Client: Enter State WaitConnAck');
        ClientState:= cs_WaitConnack;
      end;
    end;

  cs_WaitConnAck:
    begin
      if SeenConnAck then
      begin
        SeenConnAck:= false;
        LogEvent('Client:  Enter State DoSubscription_1');
        ClientState:= cs_DoSubscription_1;
      end;
      inc(ConnAckTimeOut);
      if ConnAckTimeOut > 30 then  // 3 sec
      begin
        ConnAckTimeOut:= 0;
        LogEvent('Client: Error: ConnAck time out');
        ClientState:= cs_FAILING;
      end;
    end;

  cs_DoSubscription_1:
    begin
      Client.Subscribe(Topic_Status);
      LogEvent('Client: Subscribe to ' + Topic_Status);
      ClientState:= cs_WaitSubAck_1;
    end;

  cs_WaitSubAck_1:
    begin
      Client.Subscribe(Topic_Power);
      LogEvent('Client: Subscribe to ' + Topic_Power);
      ClientState:= cs_WaitSubAck_2;
    end;

  cs_WaitSubAck_2:
    begin
      if SeenSubAck then
      begin
        SeenSubAck:= false;
        Client.Subscribe(Topic_AirQuality);
        LogEvent('Client: Subscribe to ' + Topic_AirQuality);
        ClientState:= cs_WaitSubAck_3;
      end;
    end;

  cs_WaitSubAck_3:
    begin
      if SeenSubAck then
      begin
        SeenSubAck:= false;
        Client.Subscribe(Topic_DallasTemperatures);
        LogEvent('Client: Subscribe to ' + Topic_DallasTemperatures);
        ClientState:= cs_WaitSubAck_4;
      end;
    end;

  cs_WaitSubAck_4:
    begin
      if SeenSubAck then
      begin
        SeenSubAck:= false;
        Client.Subscribe(Topic_HeartBeatAck);
        LogEvent('Client: Subscribe to ' + Topic_HeartBeatAck);
        ClientState:= cs_WaitSubAck_5;
      end;
    end;

  cs_WaitSubAck_5:
    begin
      if SeenSubAck then
      begin
        SeenSubAck:= false;
        LogEvent('Client: Enter state RUNNING');
        ClientState:= cs_Running;
      end;
    end;

  cs_Running: // In this state we can publish and receive topics
    begin
    // Publish topics
      case PublishState of
      0:begin
          if DoPublish_SetRelays then
          begin
            DoPublish_SetRelays:= false;
            Publish (Topic_SetRelays, format('%3d', [Relays]));
          end;
          if DoPublish_SetRequestLights then
          begin
            DoPublish_SetRequestLights:= false;
            Publish (Topic_SetRequestLights, format('%3d', [RequestLights]));
          end;
          PublishState:= 1;
        end;

      1..99: PublishState += 1; // give it some time

      100:begin
           Publish (Topic_HeartBeat, format('%3d', [HeartBeats]));
           Frmmain.TbxHeartBeatSent.text:= format('%3d', [HeartBeats]);
           HeartBeats += 1;
           PublishState:= 0;
         end;
      end; // case PublishState

      // Read incomming MQTT messages
      msg:= NIL;
      msg:= Client.getMessage;
      if Assigned(msg) then
      begin
        LogEvent('gotMessage: ' + msg.topic + ' Payload: ' + msg.payload);
        Topic:= msg.topic;
        Payload:= msg.payload;
        MessageAvailable:= true;
        msg.free;
      end;

      //Handle incoming messages
      if MessageAvailable then
      begin
        MessageAvailable:= false;

        if Topic = Topic_Status then with FrmMain do
        begin
          Status:= fIval(midstr(Payload, 1, 3));
          TbxStatus.text:= format('%3d', [Status]);
          VersionString:= midstr(Payload, 1, length(Payload));
          TbxVersionString.text:= VersionString;
          if (Status and $01) = $01 then
            Request1.Font.color:= clRed else Request1.Font.color:= clWhite;
          if (Status and $02) = $02 then
            Request2.Font.color:= clRed else Request2.Font.color:= clWhite;
        end;

        if Topic = Topic_Power then with FrmMain do
        begin
          //LogEvent ('Received Topic: ' + Topic);
          CH14_Amplitude:= fIval(midstr(Payload,  1, 4))/10;
          CH13_Amplitude:= fIval(midstr(Payload,  5, 4))/10;
          CH12_Amplitude:= fIval(midstr(Payload,  9, 4))/10;
          CH11_Amplitude:= fIval(midstr(Payload, 13, 4))/10;
          CH10_Amplitude:= fIval(midstr(Payload, 17, 4))/10;
          CH09_Amplitude:= fIval(midstr(Payload, 21, 4))/10;
          CH08_Amplitude:= fIval(midstr(Payload, 25, 4))/10;
          CH14_Phase:=     fIval(midstr(Payload, 29, 4));
          CH13_Phase:=     fIval(midstr(Payload, 33, 4));
          CH12_Phase:=     fIval(midstr(Payload, 37, 4));
          CH11_Phase:=     fIval(midstr(Payload, 41, 4));
          CH10_Phase:=     fIval(midstr(Payload, 45, 4));
          CH09_Phase:=     fIval(midstr(Payload, 49, 4));
          CH08_Phase:=     fIval(midstr(Payload, 53, 4));
          TbxCurrent_L1.text:= format ('%4.1f', [CH14_Amplitude]);
          TbxCurrent_L2.text:= format ('%4.1f', [CH13_Amplitude]);
          TbxCurrent_L3.text:= format ('%4.1f', [CH12_Amplitude]);
          TbxCurrent_L4.text:= format ('%4.1f', [CH11_Amplitude]);
          TbxCurrent_L5.text:= format ('%4.1f', [CH10_Amplitude]);
          TbxCurrent_L6.text:= format ('%4.1f', [CH09_Amplitude]);
          TbxCurrent_L7.text:= format ('%4.1f', [CH08_Amplitude]);
          TbxPhase_L1.text:=   format ('%4.1f', [CH14_Phase]);
          TbxPhase_L2.text:=   format ('%4.1f', [CH13_Phase]);
          TbxPhase_L3.text:=   format ('%4.1f', [CH12_Phase]);
          TbxPhase_L4.text:=   format ('%4.1f', [CH11_Phase]);
          TbxPhase_L5.text:=   format ('%4.1f', [CH10_Phase]);
          TbxPhase_L6.text:=   format ('%4.1f', [CH09_Phase]);
          TbxPhase_L7.text:=   format ('%4.1f', [CH08_Phase]);
        end;

        if Topic = Topic_AirQuality then with FrmMain do
        begin
          //LogEvent ('Received Topic: ' + Topic);
          Temperature:= fIval(midstr(Payload,  1, 5))/10;
          Hygro:=       fIval(midstr(Payload,  6, 5));
          Baro:=        fIval(midstr(Payload,  11, 5));
          CO2:=         fIval(midstr(Payload,  16, 5));
          VOC:=         fIval(midstr(Payload,  21, 5));
          P10:=         fIval(midstr(Payload,  26, 5));
          P2p5:=        fIval(midstr(Payload,  31, 5));
          TbxTemperature.text:= format ('%6.1f', [Temperature]);
          TbxHygro.text:=       format ('%4d', [Hygro]);
          TbxBaro.text:=        format ('%5d', [Baro]);
          TbxCO2.text:=         format ('%5d', [CO2]);
          TbxVOC.text:=         format ('%5d', [VOC]);
          TbxP10.text:=         format ('%5d', [P10]);
          TbxP2p5.text:=        format ('%5d', [P2p5]);
        end;

        if Topic = Topic_DallasTemperatures then with FrmMain do
        begin
          LogEvent ('Received Topic: ' + Topic);
          DallasTemperatures[0]:= fIval(midstr(Payload,  1, 5))/10;
          DallasTemperatures[1]:= fIval(midstr(Payload,  6, 5))/10;
          DallasTemperatures[2]:= fIval(midstr(Payload, 11, 5))/10;
          DallasTemperatures[3]:= fIval(midstr(Payload, 16, 5))/10;
          DallasTemperatures[4]:= fIval(midstr(Payload, 21, 5))/10;

          DallasTemperatures[5]:= fIval(midstr(Payload, 26, 5))/10;
          DallasTemperatures[6]:= fIval(midstr(Payload, 31, 5))/10;
          DallasTemperatures[7]:= fIval(midstr(Payload, 36, 5))/10;
          DallasTemperatures[8]:= fIval(midstr(Payload, 41, 5))/10;
          DallasTemperatures[9]:= fIval(midstr(Payload, 46, 5))/10;

          TbxDallasTemperatures_0.text:= format ('%6.1f', [DallasTemperatures[0]]);
          TbxDallasTemperatures_1.text:= format ('%6.1f', [DallasTemperatures[1]]);
          TbxDallasTemperatures_2.text:= format ('%6.1f', [DallasTemperatures[2]]);
          TbxDallasTemperatures_3.text:= format ('%6.1f', [DallasTemperatures[3]]);
          TbxDallasTemperatures_4.text:= format ('%6.1f', [DallasTemperatures[4]]);

          TbxDallasTemperatures_5.text:= format ('%6.1f', [DallasTemperatures[5]]);
          TbxDallasTemperatures_6.text:= format ('%6.1f', [DallasTemperatures[6]]);
          TbxDallasTemperatures_7.text:= format ('%6.1f', [DallasTemperatures[7]]);
          TbxDallasTemperatures_8.text:= format ('%6.1f', [DallasTemperatures[8]]);
          TbxDallasTemperatures_9.text:= format ('%6.1f', [DallasTemperatures[9]]);

        end;

        if Topic = Topic_HeartBeatAck then with FrmMain do
        begin
          //LogEvent ('Received Topic: ' + Topic);
          HeartBeatReceived:= fIval(Payload);
          FrmMain.TbxHeartBeatReceived.text := format('%3d', [HeartBeatReceived]);
        end;
      end;

      if ButtDisconnectPressed then
      begin
        ButtDisconnectPressed:= false;
        Client.ForceDisconnect;
        LogEvent('Client: Enter State csIDLE');
        ClientState:= cs_IDLE;
      end;
    end;
  end; // case ClientState

  // Read incomming acknowledgements
  ack:= NIL;
  ack:= Client.getMessageAck;
  if Assigned(ack) then
  begin
    case ack.messageType of
      CONNACK: begin SeenConnAck:= true; LogEvent('Client: got CONNACK'); end;
      SUBACK:  begin SeenSubAck:= true; LogEvent('Client: got SUBACK'); end;
      // As we are using QoS level 0 we won't get PubAck's
      PUBACK:  begin SeenPubAck:= true; LogEvent('Client: got PUBACK'); end;
    end;
    ack.free;
  end;

  if TimeOut > 0 then dec(TimeOut); // general TimeOut
  FrmMain.TbxTimeOut.text:= format('%4d', [TimeOut]);
end;

end.
