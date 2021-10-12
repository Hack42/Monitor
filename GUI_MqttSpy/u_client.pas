unit u_client;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, MQTT;

var
  Client: TMQTTClient;
  mq_Topic, mq_Payload: ansistring; // latest received message
  mq_DoUpdateClient, mq_MessageAvailable, mq_TerminateSession, Send: boolean;

// We list all procedures and functions in each unit exept those in the class
// In the order in which they appear in the code below
// Functions which are never called from outside are commented out

procedure mq_ConnectClient(ServerIP, ClientID: ansistring);
procedure mq_UpdateClient; // called at 10 Hz

implementation

uses
blcksock, u_logging, u_main;

type ClientStates =
 (csIDLE, csCONNECT, csWAIT_CONNECT, csRUNNING, csFAILING);

const
  DummyTopic = 'DummyTopic';
  DummyPayload = 'DummyPayload';

var
  ClientState: ClientStates;
  Msg: TMQTTMessage;
  Ack: TMQTTMessageAck;
  ConnectTimeOut: integer;

procedure mq_ConnectClient(ServerIP, ClientID: ansistring);
begin
  Client:= TMQTTClient.Create(ServerIP, 1883);
  Client.clientID:= ClientID;
  mq_TerminateSession:= false;
  ClientState:= csCONNECT;
end;

procedure mq_UpdateClient;
begin
  if not mq_DoUpdateClient then exit;
  case ClientState of
  csIDLE:
    begin
      // do nothing
    end;
  csCONNECT:
    begin
      LogEvent('Enter State CONNECT');
      // Connect to MQTT server
      ConnectTimeOut:= 0;
      Client.Connect;
      ClientState:= csWAIT_CONNECT;
    end;
  csWAIT_CONNECT:
    begin
      LogEvent('Enter State WAIT_CONNECT');
      // Can only move to RUNNING state on recieving ConnAck
      inc(ConnectTimeOut);
      if ConnectTimeOut > 30 then  // 3 sec
      begin
        LogEvent('Error: ConnAck time out.');
        ClientState:= csFAILING;
      end;
    end;
  csRUNNING:
    begin
      // For unknown reason the spy stops receiving topics after ca. 1 minute
      // Publish Dummy Topic each 30 seconds
      if Send then
      begin
        Send:= false; // only once
        LogEvent('Publish: Topic= ' + DummyTopic);
        if not Client.Publish(DummyTopic, DummyPayload) then
        begin
          LogEvent('Error: Publish Failed.');
          ClientState:= csFAILING;
        end;
      end;
    end;
  csFAILING:
    begin
      LogEvent('Enter State csFAILING');
      Client.ForceDisconnect;
      ClientState:= csIDLE;
    end;
  end;

  msg:= Client.getMessage;
  if Assigned(msg) then
  begin
    LogEvent('Topic= ' + msg.topic + ' Payload= ' + msg.payload);
    mq_Topic:= msg.topic;
    mq_Payload:= msg.payload;
    mq_MessageAvailable:= true;
    // Important to free messages here.
    msg.free;
  end;

  // Read incomming MQTT message acknowledgments
  ack:= Client.getMessageAck;
  if Assigned(ack) then
  begin
    case ack.messageType of
    CONNACK:
      begin
        LogEvent('Received CONNACK');
        if ack.returnCode = 0 then
        begin
          DoSubscriptons;
          ClientState:= csRUNNING;
	  LogEvent('Enter RUNNING state');
        end
        else ClientState:= csFAILING;
      end;
    SUBACK:
      begin
        LogEvent(format('Received SUBACK: %6d %1d', [ack.messageId, ack.qos]));
      end;
    UNSUBACK:
      begin
        LogEvent(format('Received UNSUBACK: %6d', [ack.messageId]));
      end;
    end;
  end;
  // Important to free messages here.
  ack.free;

  if mq_TerminateSession then
  begin
    mq_TerminateSession:= false;
    LogEvent ('Terminating Session');
    ClientState:= csFAILING;
  end;
end;

begin  // unit initialisation
  ClientState:= csIDLE;
end.

