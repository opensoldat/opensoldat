unit NetworkClientFae;

interface

uses
  Steam,
  FaeBase,
  FaeClient;

var
  FaePendingAuth: Pointer;

procedure ClientSendFaeResponse(Response: TFaeResponseBox);
procedure ClientHandleFaeChallenge(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  SysUtils,
  Constants,
  Game,
  LogFile,
  Net,
  NetworkUtils,
  TraceLog,
  Unit1;

procedure ClientSendFaeResponse(Response: TFaeResponseBox);
var
  ResponseMsg: TMsg_FaeResponse;
begin
  ResponseMsg.Header.ID := MsgID_FaeData;
  ResponseMsg.Response := Response;
  UDP.SendData(ResponseMsg, sizeof(ResponseMsg), k_nSteamNetworkingSend_Reliable);
end;

procedure ClientHandleFaeChallenge(NetMessage: PSteamNetworkingMessage_t);
var
  Msg: PMsg_FaeChallenge;
  Challenge: PFaeChallenge;
  Response: TFaeResponseBox;
begin
  if not VerifyPacket(sizeof(TMsg_FaeChallenge), NetMessage^.m_cbSize, MsgID_FaeData) then
    Exit;

  if FaePendingAuth <> nil then
    FaeAuthCancel(FaePendingAuth);

  Msg := PMsg_FaeChallenge(NetMessage^.m_pData);
  Challenge := @Msg^.Challenge;

  if Msg.InOrder = 1 then
  begin
    // Synchronously process request (we must reply before sending our player info).
    // This might add a few milliseconds of delay while Fae does its thing, but the user won't
    // notice this as no frames are being rendered yet.
    if not NetEncActive then
    begin
      // NOTE that FaeAuthSync may fail, in which case NetEncKey is zeroed out. However, we're
      // not going to send any encrypted packets anyway if the server rejects us, hence we ignore
      // failure of FaeAuthSync.
      FaeAuthSync(Challenge, Response, @NetEncKey);
      NetEncActive := True;
    end
    else begin
      FaeAuthSync(Challenge, Response, nil);
    end;
    ClientSendFaeResponse(Response);
    //Debug('Sending sync. Fae response');
  end
  else begin
    FaePendingAuth := FaeAuthSubmit(Challenge, 0);
    //Debug('Pending async. Fae response');
  end;
end;

end.
