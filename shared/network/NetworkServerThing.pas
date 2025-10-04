{*************************************************************}
{                                                             }
{       NetworkServerThing Unit for OpenSoldat                }
{                                                             }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit NetworkServerThing;

interface

uses
  // Library units
  {$IFDEF SERVER}
    Steam,
  {$ENDIF}

  // Helper units
  Vector,

  // Project units
  Constants,
  Net,
  Sprites;


{$IFDEF SERVER}
procedure ServerThingSnapshot(ToNum: Byte);
procedure ServerThingMustSnapshot(ThingNum: Byte);
{$ENDIF}
procedure ServerThingMustSnapshotOnConnect({$IFDEF SERVER}ToNum: Byte{$ENDIF});
{$IFDEF SERVER}
procedure ServerThingMustSnapshotOnConnectTo(ThingNum, ToNum: Byte);
procedure ServerThingTaken(ThingNum, Who: Byte);
procedure ServerHandleRequestThing(NetMessage: PSteamNetworkingMessage_t);
{$ENDIF}


implementation

uses
  {$IFDEF SERVER}
    // Helper units
    Calc,

    // Project units
    NetworkUtils,
    Server,
  {$ELSE}
    Client,
  {$ENDIF}
  Demo,
  Game;


{$IFDEF SERVER}
procedure ServerThingSnapshot(ToNum: Byte);
var
  ThingMsg: TMsg_ServerThingSnapshot;
  i, j: Integer;
  Send: Boolean;
begin
  if ToNum > High(Sprite) then
    Exit;

  for i := Low(Thing) to High(Thing) do
  begin
    if not Thing[i].Active then
      Continue;
    if Thing[i].Style = OBJECT_PARACHUTE then
      Continue;

    if (Thing[i].Style < OBJECT_USSOCOM) or  // always snapshot flags
      (((not Thing[i].StaticType) or (Thing[i].Style = OBJECT_STATIONARY_GUN)) and
      (PointVisible(Thing[i].Skeleton.Pos[1].X, Thing[i].Skeleton.Pos[1].Y, ToNum))) then
    begin
      ThingMsg.Header.ID := MsgID_ServerThingSnapshot;
      // assign thing values to ThingMsg
      ThingMsg.Num := i;
      for j := 1 to 4 do
      begin
        ThingMsg.Pos[j].X    := Thing[i].Skeleton.Pos[j].X;
        ThingMsg.Pos[j].Y    := Thing[i].Skeleton.Pos[j].Y;
        ThingMsg.OldPos[j].X := Thing[i].Skeleton.OldPos[j].X;
        ThingMsg.OldPos[j].Y := Thing[i].Skeleton.OldPos[j].Y;
      end;

      ThingMsg.Owner         := Thing[i].Owner;
      ThingMsg.Style         := Thing[i].Style;
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
        UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[ToNum].Player.peer,
          k_nSteamNetworkingSend_Unreliable);
      end;
    end;
  end;
end;

procedure ServerThingMustSnapshot(ThingNum: Byte);
var
  ThingMsg: TMsg_ServerThingMustSnapshot;
  i: Integer;
begin
  if ThingNum > High(Thing) then
    Exit;
  if (Thing[ThingNum].Style = OBJECT_PARACHUTE) then
    Exit;

  ThingMsg := Default(TMsg_ServerThingMustSnapshot);
  ThingMsg.Header.ID := MsgID_ServerThingMustSnapshot;
  // assign thing values to ThingMsg
  ThingMsg.Num := ThingNum;
  for i := 1 to 4 do
  begin
    ThingMsg.Pos[i].X    := Thing[i].Skeleton.Pos[i].X;
    ThingMsg.Pos[i].Y    := Thing[i].Skeleton.Pos[i].Y;
    ThingMsg.OldPos[i].X := Thing[i].Skeleton.OldPos[i].X;
    ThingMsg.OldPos[i].Y := Thing[i].Skeleton.OldPos[i].Y;
  end;
  ThingMsg.Timeout := Thing[i].Timeout;
  if Thing[ThingNum].Timeout < 1 then
    ThingMsg.Timeout := 1;
  ThingMsg.Owner         := Thing[ThingNum].Owner;
  ThingMsg.Style         := Thing[ThingNum].Style;
  ThingMsg.HoldingSprite := Thing[ThingNum].HoldingSprite;

  for i := Low(Sprite) to High(Sprite) do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[i].Player.peer,
        k_nSteamNetworkingSend_Unreliable);
end;
{$ENDIF}

procedure ServerThingMustSnapshotOnConnect({$IFDEF SERVER}ToNum: Byte{$ENDIF});
var
  ThingMsg: TMsg_ServerThingMustSnapshot;
  i, j: Integer;
begin
  {$IFDEF SERVER}
  if ToNum > High(Sprite) then
    Exit;
  {$ENDIF}

  for i := Low(Thing) to High(Thing) do
  begin
    if not Thing[i].Active then
      Continue;

    if ((Thing[i].Style < OBJECT_USSOCOM) or (Thing[i].Style > OBJECT_MINIGUN)) and
        (Thing[i].Style <> OBJECT_PARACHUTE) then
    begin
     ThingMsg.Header.ID := MsgID_ServerThingMustSnapshot;
     // assign thing values to ThingMsg
     ThingMsg.Num := i;
     for j := 1 to 4 do
     begin
       ThingMsg.Pos[j].X    := Thing[i].Skeleton.Pos[j].X;
       ThingMsg.Pos[j].Y    := Thing[i].Skeleton.Pos[j].Y;
       ThingMsg.OldPos[j].X := Thing[i].Skeleton.OldPos[j].X;
       ThingMsg.OldPos[j].Y := Thing[i].Skeleton.OldPos[j].Y;
     end;
     ThingMsg.Timeout := SmallInt(Thing[i].Timeout);
     if Thing[i].Timeout < 1 then
       ThingMsg.Timeout := 1;
     ThingMsg.Owner         := Thing[i].Owner;
     ThingMsg.Style         := Thing[i].Style;
     ThingMsg.HoldingSprite := Thing[i].HoldingSprite;

     {$IFDEF SERVER}
     UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[ToNum].Player.peer,
       k_nSteamNetworkingSend_Unreliable);
     {$ELSE}
     DemoRecorder.SaveRecord(ThingMsg, sizeof(ThingMsg));
     {$ENDIF}
    end;
  end;
end;

{$IFDEF SERVER}
procedure ServerThingMustSnapshotOnConnectTo(ThingNum, ToNum: Byte);
var
  ThingMsg: TMsg_ServerThingMustSnapshot;
  i: Integer;
begin
  if ThingNum > High(Thing) then
    Exit;
  if ToNum > High(Sprite) then
    Exit;
  if (Thing[ThingNum].Style = OBJECT_PARACHUTE) then
    Exit;

  ThingMsg.Header.ID := MsgID_ServerThingMustSnapshot;
  // assign thing values to ThingMsg
  ThingMsg.Num := ThingNum;
  for i := 1 to 4 do
  begin
    ThingMsg.Pos[i].X    := Thing[ThingNum].Skeleton.Pos[i].X;
    ThingMsg.Pos[i].Y    := Thing[ThingNum].Skeleton.Pos[i].Y;
    ThingMsg.OldPos[i].X := Thing[ThingNum].Skeleton.OldPos[i].X;
    ThingMsg.OldPos[i].Y := Thing[ThingNum].Skeleton.OldPos[i].Y;
  end;
  ThingMsg.Timeout := Thing[ThingNum].Timeout;
  if Thing[ThingNum].Timeout < 1 then
    ThingMsg.Timeout := 1;
  ThingMsg.Owner         := Thing[ThingNum].Owner;
  ThingMsg.Style         := Thing[ThingNum].Style;
  ThingMsg.HoldingSprite := Thing[ThingNum].HoldingSprite;

  UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[ToNum].Player.peer,
    k_nSteamNetworkingSend_Unreliable);
end;

procedure ServerThingTaken(ThingNum, Who: Byte);
var
  ThingMsg: TMsg_ServerThingTaken;
  i: Byte;
begin
  if ThingNum > High(Thing) then
    Exit;
  if (Who <> 255) and (Who > High(Sprite)) then
    Exit;

  ThingMsg.Header.ID := MsgID_ThingTaken;
  ThingMsg.Num := Thing[ThingNum].Num;
  ThingMsg.Who := Who;

  ThingMsg.Style     := Thing[ThingNum].Style;
  ThingMsg.AmmoCount := Thing[ThingNum].AmmoCount;

  for i := Low(Sprite) to High(Sprite) do
    if (Sprite[i].Active) and (Sprite[i].Player.ControlMethod = HUMAN) then
      UDP.SendData(ThingMsg, sizeof(ThingMsg), Sprite[i].Player.peer,
        k_nSteamNetworkingSend_Unreliable);
end;

procedure ServerHandleRequestThing(NetMessage: PSteamNetworkingMessage_t);
var
  Msg: PMsg_RequestThing;
  Player: TPlayer;
begin
  if not VerifyPacket(sizeof(TMsg_RequestThing), NetMessage^.m_cbSize,
    MsgID_RequestThing) then
    Exit;

  Msg := PMsg_RequestThing(NetMessage^.m_pData);
  Player := TPlayer(NetMessage^.m_nConnUserData);
  ServerThingMustSnapshotOnConnectTo(Msg.ThingID, Player.SpriteNum);
end;
{$ENDIF}

end.
