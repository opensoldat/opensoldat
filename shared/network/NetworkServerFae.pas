unit NetworkServerFae;

interface

uses Steam;

procedure ServerSendFaeChallenge(Peer: HSteamNetConnection; InOrder: Boolean);
procedure ServerHandleFaeResponse(NetMessage: PSteamNetworkingMessage_t);

implementation

uses
  SysUtils,
  Constants,
  Game,
  FaeBase,
  FaeRemoteAttestation,
  Net,
  NetworkServerConnection,
  NetworkServerMessages,
  NetworkUtils,
  TraceLog,
  Unit1;

function IsFaeGameDataValid(Response: TFaeResponse): Boolean;
begin
  // TODO check that Response.GameKey matches Soldat's signature public key
  // TODO check that Response.GameVersion matches the client version we expect
  Response := Response;
  Result := True;
end;

procedure KickForFaeViolation(Player: TPlayer; Reason: String);
var
  Name: WideString;
  MsgText: WideString;
begin
  Name := WideString(Player.Name);
  if Name = '' then
    Name := 'Unnamed player ' + WideString(Player.IP) + ':'
      + WideString(IntToStr(Player.Port));
  MsgText := Name + ' removed by anti-cheat: ' + WideString(Reason);
  MainConsole.Console('[AC] ' + MsgText, CLIENT_MESSAGE_COLOR);
  Player.FaeKicked := True;
  if Player.SpriteNum <> 0 then
  begin
    ServerSendStringMessage('/say ' + MsgText, ALL_PLAYERS, 255, MSGTYPE_PUB);
    KickPlayer(Player.SpriteNum, False, KICK_AC, 0, 'Anti-Cheat: ' + Reason);
  end else
  begin
    ServerSendUnAccepted(Player.Peer, ANTICHEAT_REJECTED, Reason);
  end;
end;

procedure ServerSendFaeChallenge(Peer: HSteamNetConnection; InOrder: Boolean);
var
  Player: TPlayer;
  ChallengeMsg: TMsg_FaeChallenge;
begin
  Player := TPlayer(Peer^.data);
  FaeInitSecret(@Player.FaeSecret);
  FaeInitChallenge(@Player.FaeSecret, ChallengeMsg.Challenge);

  // The first Fae request's secret is used for deriving a session key.
  // The client recovers this key by calling FaeAuthSync.
  if not Player.EncActive then
    FaeDeriveKey(@Player.FaeSecret, @Player.EncKey);

  Player.FaeResponsePending := True;

  ChallengeMsg.InOrder := Byte(InOrder);
  ChallengeMsg.Header.ID := MsgID_FaeData;
  UDP.SendData(ChallengeMsg, sizeof(ChallengeMsg), Peer, k_nSteamNetworkingSend_Reliable);
end;

procedure ServerHandleFaeResponse(NetMessage: PSteamNetworkingMessage_t);
var
  Player: TPlayer;
  ResponseMsg: PMsg_FaeResponse;
  OuterStatus: Cardinal;
  Reason: String;
  ValidationErr: Integer;
  ValidatedResp: TFaeResponse;
begin
  Trace('[AC] Received response');

  if not VerifyPacket(sizeof(TMsg_FaeResponse), NetMessage^.m_cbSize, MsgID_FaeData) then
    Exit;

  Player := TPlayer(NetMessage^.m_nConnUserData);

  if not Player.FaeResponsePending then
  begin
    Debug('[AC] Warning: Discarding unexpected response');
    Exit;
  end;

  Trace('[AC] Expected response');

  ResponseMsg := PMsg_FaeResponse(NetMessage^.m_pData);
  OuterStatus := ResponseMsg^.Response.OuterStatus;

  if OuterStatus <> 0 then
  begin
    // The response is well-formed, but the AC cannot process our secret, for example because it
    // is out-of-date or some kind of initialization failed.
    Trace('[Fae] But the client refused to process our secret');
    if (OuterStatus and $80000000) <> 0 then
      Reason := 'Ban ID #' + IntToStr(OuterStatus and $7FFFFFFF)
    else
    case OuterStatus of
      1: Reason := 'Test Error'; // test by setting client's env.-var: FAE_TESTMODE=status1
      2: Reason := 'Soldat Restart Required';
      3: Reason := 'Anti-Cheat Activation Failed';
      else Reason := 'Initialization Error #' + IntToStr(OuterStatus);
    end;
    KickForFaeViolation(Player, Reason);
  end
  else begin
    ValidationErr := FaeCheck(@Player.FaeSecret, @ResponseMsg^.Response, ValidatedResp);
    if ValidationErr <> FAECHECK_OK then
    begin
      case ValidationErr of
        FAECHECK_ERR_INVALID: Reason := 'Corrupted Message';
        FAECHECK_ERR_CLOCK: Reason := 'Client/Server Clock Mismatch';
        else Reason := 'Validation Error #' + IntToStr(ValidationErr);
      end;
      KickForFaeViolation(Player, Reason);
    end
    else begin
      // This was a valid response for our secret -- we may now read from ValidatedResp
      Trace('[Fae] And it was valid, processing');
      if not IsFaeGameDataValid(ValidatedResp) then begin
        // The client's fae.db doesn't contain the info this server build expects
        KickForFaeViolation(Player, 'Client Build Info Mismatch')
      end
      else if ValidatedResp.Status <> 0 then
      begin
        // Fae found some issue with the game client and we should boot the player.
        // You may test this by setting the client's environment variable FAE_TESTMODE=status2,
        // which results in 'Test Code #1000'.
        Reason := PAnsiChar(@ValidatedResp.StatusStr[1]);
        if Length(Reason) = 0 then
          Reason := 'Code #' + IntToStr(ValidatedResp.Status)
        else
          Reason := Reason + ' #' + IntToStr(ValidatedResp.Status);
        KickForFaeViolation(Player, Reason);
      end
      else begin
        // Everything is fine. Reset the ticks counter so that the player is not kicked.
        Trace('[AC] All good, timer reset');
        Player.FaeResponsePending := False;
        Player.FaeTicks := 0;

        // Send encrypted packets from now on.
        if not Player.EncActive then
        begin
          Debug('[AC] Network encryption enabled for ' + player.IP + ':' + IntToStr(player.Port));
          Player.EncActive := True;
        end;
      end;
    end;
  end;
end;

end.
