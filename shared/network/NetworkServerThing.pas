unit NetworkServerThing;

interface

uses
  // helper units
  Vector,

  // OpenSoldat units
  {$IFDEF SERVER}Steam,{$ENDIF}
  Net, Sprites, Constants;

{$IFDEF SERVER}
procedure ServerThingSnapshot(ToNum: Byte);
procedure ServerThingMustSnapshot(i: Byte);
{$ENDIF}
procedure ServerThingMustSnapshotOnConnect({$IFDEF SERVER}ToNum: Byte{$ENDIF});
{$IFDEF SERVER}
procedure ServerThingMustSnapshotOnConnectTo(i, ToNum: Byte);
procedure ServerThingTaken(i, w: Byte);
procedure ServerHandleRequestThing(NetMessage: PSteamNetworkingMessage_t);
{$ENDIF}

implementation

uses
  {$IFDEF SERVER}Server, NetworkUtils, Calc, {$ELSE}Client,{$ENDIF} Game, Demo;

{$IFDEF SERVER}
procedure ServerThingSnapshot(ToNum: Byte);
var
  ThingMsg: TMsg_ServerThingSnapshot;
  i, j: Integer;
  Send: Boolean;
begin
  for i := 1 to MAX_THINGS do
    if (Thing[i].Active) and (Thing[i].Style <> OBJECT_PARACHUTE) and
      ((not Thing[i].StaticType) or ((Thing[i].Style < OBJECT_USSOCOM) or
      (Thing[i].Style = OBJECT_STATIONARY_GUN))) and
      ((PointVisible(Thing[i].Skeleton.Pos[1].X,
      Thing[i].Skeleton.Pos[1].Y, ToNum)) or
      (Thing[i].Style < OBJECT_USSOCOM)) then
    begin
      ThingMsg.Header.ID := MsgID_ServerThingSnapshot;
      // assign thing values to ThingMsg
      ThingMsg.Num := i;
      for j := 1 to 4 do
      begin
        ThingMsg.Pos[j].X := Thing[i].Skeleton.Pos[j].X;
        ThingMsg.Pos[j].Y := Thing[i].Skeleton.Pos[j].Y;
        ThingMsg.OldPos[j].X := Thing[i].Skeleton.OldPos[j].X;
        ThingMsg.OldPos[j].Y := Thing[i].Skeleton.OldPos[j].Y;
      end;

      ThingMsg.Owner := Thing[i].Owner;
      ThingMsg.Style := Thing[i].Style;
      ThingMsg.HoldingSprite := Thing[i].HoldingSprite;

      // send only if moving
      Send := False;
      if (Distance(ThingMsg.Pos[1].X, ThingMsg.Pos[1].Y, ThingMsg.OldPos[1].X,
        ThingMsg.OldPos[1].Y) > MINMOVEDELTA) or
        (Distance(ThingMsg.Pos[2].X, ThingMsg.Pos[2].Y, ThingMsg.OldPos[2].X,
        ThingMsg.OldPos[2].Y) > MINMOVEDELTA) then
        Send := True;

      if MainTickCounter mod 2 = 0 then
      begin
        if Thing[i].Style < OBJECT_USSOCOM then
          Send := True;
        if Thing[i].Style = OBJECT_RAMBO_BOW then
          Send := True;
      end;

      if Send then
      begin
        UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[ToNum].Player.peer, k_nSteamNetworkingSend_Unreliable);
      end;
    end;
end;

procedure ServerThingMustSnapshot(i: Byte);
var
  ThingMsg: TMsg_ServerThingMustSnapshot;
  j: Integer;
begin
  ThingMsg := Default(TMsg_ServerThingMustSnapshot);
  if (Thing[i].Style = OBJECT_PARACHUTE) then
    Exit;

  ThingMsg.Header.ID := MsgID_ServerThingMustSnapshot;
  // assign thing values to ThingMsg
  ThingMsg.Num := i;
  for j := 1 to 4 do
  begin
    ThingMsg.Pos[j].X := Thing[i].Skeleton.Pos[j].X;
    ThingMsg.Pos[j].Y := Thing[i].Skeleton.Pos[j].Y;
    ThingMsg.OldPos[j].X := Thing[i].Skeleton.OldPos[j].X;
    ThingMsg.OldPos[j].Y := Thing[i].Skeleton.OldPos[j].Y;
  end;
  ThingMsg.Timeout := Thing[i].Timeout;
  if Thing[i].Timeout < 1 then
    ThingMsg.Timeout := 1;
  ThingMsg.Owner := Thing[i].Owner;
  ThingMsg.Style := Thing[i].Style;
  ThingMsg.HoldingSprite := Thing[i].HoldingSprite;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Unreliable);
end;
{$ENDIF}

procedure ServerThingMustSnapshotOnConnect({$IFDEF SERVER}ToNum: Byte{$ENDIF});
var
  ThingMsg: TMsg_ServerThingMustSnapshot;
  i, j: Integer;
begin
  for i := 1 to MAX_THINGS do
    if Thing[i].Active then
      if ((Thing[i].Style < OBJECT_USSOCOM) or (Thing[i].Style > OBJECT_MINIGUN)) and
        (Thing[i].Style <> OBJECT_PARACHUTE) then
      begin
        ThingMsg.Header.ID := MsgID_ServerThingMustSnapshot;
        // assign thing values to ThingMsg
        ThingMsg.Num := i;
        for j := 1 to 4 do
        begin
          ThingMsg.Pos[j].X := Thing[i].Skeleton.Pos[j].X;
          ThingMsg.Pos[j].Y := Thing[i].Skeleton.Pos[j].Y;
          ThingMsg.OldPos[j].X := Thing[i].Skeleton.OldPos[j].X;
          ThingMsg.OldPos[j].Y := Thing[i].Skeleton.OldPos[j].Y;
        end;
        ThingMsg.Timeout := SmallInt(Thing[i].Timeout);
        if Thing[i].Timeout < 1 then
          ThingMsg.Timeout := 1;
        ThingMsg.Owner := Thing[i].Owner;
        ThingMsg.Style := Thing[i].Style;
        ThingMsg.HoldingSprite := Thing[i].HoldingSprite;

        {$IFDEF SERVER}
        UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[ToNum].Player.peer, k_nSteamNetworkingSend_Unreliable);
        {$ELSE}
        DemoRecorder.SaveRecord(ThingMsg, sizeof(ThingMsg));
        {$ENDIF}
      end;
end;

{$IFDEF SERVER}
procedure ServerThingMustSnapshotOnConnectTo(i, ToNum: Byte);
var
  ThingMsg: TMsg_ServerThingMustSnapshot;
  j: Integer;
begin
  if (Thing[i].Style = OBJECT_PARACHUTE) then
    Exit;

  ThingMsg.Header.ID := MsgID_ServerThingMustSnapshot;
  // assign thing values to ThingMsg
  ThingMsg.Num := i;
  for j := 1 to 4 do
  begin
    ThingMsg.Pos[j].X := Thing[i].Skeleton.Pos[j].X;
    ThingMsg.Pos[j].Y := Thing[i].Skeleton.Pos[j].Y;
    ThingMsg.OldPos[j].X := Thing[i].Skeleton.OldPos[j].X;
    ThingMsg.OldPos[j].Y := Thing[i].Skeleton.OldPos[j].Y;
  end;
  ThingMsg.Timeout := Thing[i].Timeout;
  if Thing[i].Timeout < 1 then
    ThingMsg.Timeout := 1;
  ThingMsg.Owner := Thing[i].Owner;
  ThingMsg.Style := Thing[i].Style;
  ThingMsg.HoldingSprite := Thing[i].HoldingSprite;

  UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[ToNum].Player.peer, k_nSteamNetworkingSend_Unreliable);
end;

procedure ServerThingTaken(i, w: Byte);
var
  ThingMsg: TMsg_ServerThingTaken;
begin
  ThingMsg.Header.ID := MsgID_ThingTaken;
  ThingMsg.Num := Thing[i].Num;
  ThingMsg.Who := w;

  ThingMsg.Style := Thing[i].Style;
  ThingMsg.AmmoCount := Thing[i].AmmoCount;

  for i := 1 to MAX_PLAYERS do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[i].Player.peer, k_nSteamNetworkingSend_Unreliable);
end;

procedure ServerHandleRequestThing(NetMessage: PSteamNetworkingMessage_t);
var
  Msg: PMsg_RequestThing;
  Player: TPlayer;
begin
  if not VerifyPacket(sizeof(TMsg_RequestThing), NetMessage^.m_cbSize, MsgID_RequestThing) then
    Exit;
  Msg := PMsg_RequestThing(NetMessage^.m_pData);
  Player := TPlayer(NetMessage^.m_nConnUserData);
  ServerThingMustSnapshotOnConnectTo(Msg.ThingID, Player.SpriteNum);
end;
{$ENDIF}

end.

