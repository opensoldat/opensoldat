{*******************************************************}
{                                                       }
{       Animation Unit for OPENSOLDAT                   }
{       Based on Strike of the Dragon ENGINE            }
{       by Michal Marcinkowski                          }
{                                                       }
{       Copyright (c) 2001 Michal Marcinkowski          }
{                                                       }
{*******************************************************}

unit Anims;

interface

uses
  Vector;

const
  MAX_POS_INDEX    = 20;
  MAX_FRAMES_INDEX = 40;

type
  TFrame = record
    Pos: array[1..MAX_POS_INDEX] of TVector3;
  end;

  TAnimation = object
    ID: Integer;
    Frames: array[1..MAX_FRAMES_INDEX] of TFrame;
    NumFrames: Integer;
    Speed, Count: Integer;
    CurrFrame: Integer;
    Loop: Boolean;
  public
    procedure DoAnimation;
    procedure LoadFromFile(Filename: string);
    function  CheckSum: Integer;
  end;

  procedure LoadAnimObjects(ModDir: string);

implementation

uses
  {$IFDEF SERVER}
  Server,
  {$ELSE}
  Client,
  {$ENDIF}
  SysUtils, TraceLog, PhysFS, Game, LogFile;

const
  SCALE = 3;

procedure TAnimation.DoAnimation;
begin
  Inc(Count);
  if Count = Speed then
  begin
    Count := 0;
    Inc(CurrFrame);
    if CurrFrame > NumFrames then
      if Loop then
        CurrFrame := 1
      else
        CurrFrame := NumFrames;
  end;
end;

procedure TAnimation.LoadFromFile(Filename: string);
var
  F: PHYSFS_File;
  r1: string = '';
  r2: string = '';
  r3: string = '';
  r4: string = '';
  p: Integer;
begin
  if not PHYSFS_Exists(PChar(Filename)) then
    Exit;

  NumFrames := 1;

  // default settings
  Loop  := False;
  Speed := 1;
  Count := 0;

  F := PHYSFS_openRead(PChar(Filename));

  PhysFS_ReadLN(F, r1);
  While r1 <> 'ENDFILE' do
  begin
    if r1 = 'NEXTFRAME' then
    begin
      if NumFrames = MAX_FRAMES_INDEX then
      begin
        Debug('Corrupted frame index: ' + Filename);
        Break;
      end;

      Inc(NumFrames);
    end
    else
    begin
      PhysFS_ReadLN(F, r2);  // X
      PhysFS_ReadLN(F, r3);  // Y
      PhysFS_ReadLN(F, r4);  // Z

      p := StrToIntDef(r1, 0);
      if (p >= 1) and (p <= MAX_POS_INDEX) then
      begin
        // TODO: check if this is correct
        Frames[NumFrames].Pos[p].X := -SCALE * StrToFloat(r2) / 1.1;
        Frames[NumFrames].Pos[p].Y := -SCALE * StrToFloat(r4);
      end
      else
        Debug('Corrupted Index (' + r1 + '): ' + Filename);
    end;

    PhysFS_ReadLN(F, r1);
  end;
  PHYSFS_close(F);
  CurrFrame := 1;
end;

function TAnimation.CheckSum: Integer;
var
  chk: Single;
  i, j: Integer;
begin
  chk := 0.5;

  for i := 1 to NumFrames do
    for j := 1 to 20 do
    begin
      chk := chk + Frames[i].Pos[j].X;
      chk := chk + Frames[i].Pos[j].Y;
      chk := chk + Frames[i].Pos[j].Z;
    end;

  Result := Trunc(chk);
end;

// TODO: add file missing checks
// TODO: translate filenames into english
procedure LoadAnimObjects(ModDir: string);
begin
  AddLineToLogFile(GameLog, 'Loading Animations. ' + ModDir, ConsoleLogFileName);

  // Anims load

  // TODO: use english filenames
  // stand
  Stand.LoadFromFile(ModDir + 'anims/stoi.poa');
  Stand.ID := 0;
  Stand.Loop := True;
  Stand.Speed := 3;

  // run
  Run.LoadFromFile(ModDir + 'anims/biega.poa');
  Run.ID := 1;
  Run.Loop := True;

  // run back
  RunBack.LoadFromFile(ModDir + 'anims/biegatyl.poa');
  RunBack.ID := 2;
  RunBack.Loop := True;

  // jump
  Jump.LoadFromFile(ModDir + 'anims/skok.poa'); ;
  Jump.ID := 3;

  // jump to side
  JumpSide.LoadFromFile(ModDir + 'anims/skokwbok.poa');
  JumpSide.ID := 4;

  // fall
  Fall.LoadFromFile(ModDir + 'anims/spada.poa');
  Fall.ID := 5;

  // crouch
  Crouch.LoadFromFile(ModDir + 'anims/kuca.poa');
  Crouch.ID := 6;

  // crouch walk
  CrouchRun.LoadFromFile(ModDir + 'anims/kucaidzie.poa');
  CrouchRun.ID := 7;
  CrouchRun.Loop := True;
  CrouchRun.Speed := 2;

  // reloading
  Reload.LoadFromFile(ModDir + 'anims/laduje.poa');
  Reload.ID := 8;
  Reload.Speed := 2;

  // throw
  Throw.LoadFromFile(ModDir + 'anims/rzuca.poa');
  Throw.ID := 9;
  Throw.Speed := 1;

  // throwback?
  Recoil.LoadFromFile(ModDir + 'anims/odrzut.poa');
  Recoil.ID := 10;

  // throwback2?
  SmallRecoil.LoadFromFile(ModDir + 'anims/odrzut2.poa');
  SmallRecoil.ID := 11;

  Shotgun.LoadFromFile(ModDir + 'anims/shotgun.poa');
  Shotgun.ID := 12;

  ClipOut.LoadFromFile(ModDir + 'anims/clipout.poa');
  ClipOut.ID := 13;
  ClipOut.Speed := 3;

  ClipIn.LoadFromFile(ModDir + 'anims/clipin.poa');
  ClipIn.ID := 14;
  ClipIn.Speed := 3;

  SlideBack.LoadFromFile(ModDir + 'anims/slideback.poa');
  SlideBack.ID := 15;
  SlideBack.Speed := 2;
  SlideBack.Loop := True;

  Change.LoadFromFile(ModDir + 'anims/change.poa');
  Change.ID := 16;
  Change.Loop := False;

  // throwout?
  ThrowWeapon.LoadFromFile(ModDir + 'anims/wyrzuca.poa');
  ThrowWeapon.ID := 17;
  ThrowWeapon.Loop := False;

  // without weapon
  WeaponNone.LoadFromFile(ModDir + 'anims/bezbroni.poa');
  WeaponNone.ID := 18;
  WeaponNone.Speed := 3;

  // punch
  Punch.LoadFromFile(ModDir + 'anims/bije.poa');
  Punch.ID := 19;
  Punch.Loop := False;

  // shoot?
  ReloadBow.LoadFromFile(ModDir + 'anims/strzala.poa');
  ReloadBow.ID := 20;

  Barret.LoadFromFile(ModDir + 'anims/barret.poa');
  Barret.ID := 21;
  Barret.Speed := 9;

  // jump?
  Roll.LoadFromFile(ModDir + 'anims/skokdolobrot.poa');
  Roll.ID := 22;
  Roll.Speed := 1;

  // jump? back
  RollBack.LoadFromFile(ModDir + 'anims/skokdolobrottyl.poa');
  RollBack.ID := 23;
  RollBack.Speed := 1;

  // crouch walk back
  CrouchRunBack.LoadFromFile(ModDir + 'anims/kucaidzietyl.poa');
  CrouchRunBack.ID := 24;
  CrouchRunBack.Loop := True;
  CrouchRunBack.Speed := 2;

  Cigar.LoadFromFile(ModDir + 'anims/cigar.poa');
  Cigar.ID := 25;
  Cigar.Speed := 3;

  Match.LoadFromFile(ModDir + 'anims/match.poa');
  Match.ID := 26;
  Match.Speed := 3;

  Smoke.LoadFromFile(ModDir + 'anims/smoke.poa');
  Smoke.ID := 27;
  Smoke.Speed := 4;

  Wipe.LoadFromFile(ModDir + 'anims/wipe.poa');
  Wipe.ID := 28;
  Wipe.Speed := 4;

  // ?
  Groin.LoadFromFile(ModDir + 'anims/krocze.poa');
  Groin.ID := 29;
  Groin.Speed := 2;

  // ?
  Piss.LoadFromFile(ModDir + 'anims/szcza.poa');
  Piss.ID := 30;
  Piss.Speed := 8;

  // ?
  Mercy.LoadFromFile(ModDir + 'anims/samo.poa');
  Mercy.ID := 31;
  Mercy.Speed := 3;

  // ?
  Mercy2.LoadFromFile(ModDir + 'anims/samo2.poa');
  Mercy2.ID := 32;
  Mercy2.Speed := 3;

  // take off hat
  TakeOff.LoadFromFile(ModDir + 'anims/takeoff.poa');
  TakeOff.ID := 33;
  TakeOff.Speed := 2;

  // lying
  Prone.LoadFromFile(ModDir + 'anims/lezy.poa');
  Prone.ID := 34;
  Prone.Speed := 1;

  // happy/victory
  Victory.LoadFromFile(ModDir + 'anims/cieszy.poa');
  Victory.ID := 35;
  Victory.Speed := 3;

  // aiming
  Aim.LoadFromFile(ModDir + 'anims/celuje.poa');
  Aim.ID := 36;
  Aim.Speed := 2;

  // top?
  HandsUpAim.LoadFromFile(ModDir + 'anims/gora.poa');
  HandsUpAim.ID := 37;
  HandsUpAim.Speed := 2;

  // lying walk
  ProneMove.LoadFromFile(ModDir + 'anims/lezyidzie.poa');
  ProneMove.ID := 38;
  ProneMove.Loop := True;
  ProneMove.Speed := 2;

  // stand up
  GetUp.LoadFromFile(ModDir + 'anims/wstaje.poa');
  GetUp.ID := 39;
  GetUp.Speed := 1;

  // aim throw?
  AimRecoil.LoadFromFile(ModDir + 'anims/celujeodrzut.poa');
  AimRecoil.ID := 40;
  AimRecoil.Speed := 1;

  // top throw?
  HandsUpRecoil.LoadFromFile(ModDir + 'anims/goraodrzut.poa');
  HandsUpRecoil.ID := 41;
  HandsUpRecoil.Speed := 1;

  // ?
  Melee.LoadFromFile(ModDir + 'anims/kolba.poa');
  Melee.ID := 42;
  Melee.Speed := 1;

  // ?
  Own.LoadFromFile(ModDir + 'anims/rucha.poa');
  Own.ID := 43;
  Own.Speed := 3;

  AddLineToLogFile(GameLog, 'Loading objects.', ConsoleLogFileName);

  SpriteParts.Destroy;
  SpriteParts.TimeStep := 1;
  SpriteParts.Gravity := GRAV;
  SpriteParts.EDamping := 0.99;
  GostekSkeleton.Destroy;
  GostekSkeleton.LoadPOObject('objects/gostek.po', SCALE);
  GostekSkeleton.TimeStep := 1;
  GostekSkeleton.Gravity := 1.06 * GRAV;
  GostekSkeleton.VDamping := 0.997;

  BoxSkeleton.Destroy;
  BoxSkeleton.LoadPOObject('objects/kit.po', 2.15);
  BoxSkeleton.TimeStep := 1;

  BulletParts.Destroy;
  BulletParts.TimeStep := 1;
  BulletParts.Gravity := GRAV * 2.25;
  BulletParts.EDamping := 0.99;

  SparkParts.Destroy;
  SparkParts.TimeStep := 1;
  SparkParts.Gravity := GRAV / 1.4;
  SparkParts.EDamping := 0.998;

  FlagSkeleton.LoadPOObject('objects/flag.po', 4.0);
  ParaSkeleton.LoadPOObject('objects/para.po', 5.0);
  StatSkeleton.LoadPOObject('objects/stat.po', 4.0);
  RifleSkeleton10.LoadPOObject('objects/karabin.po', 1.0);
  RifleSkeleton11.LoadPOObject('objects/karabin.po', 1.1);
  RifleSkeleton18.LoadPOObject('objects/karabin.po', 1.8);
  RifleSkeleton22.LoadPOObject('objects/karabin.po', 2.2);
  RifleSkeleton28.LoadPOObject('objects/karabin.po', 2.8);
  RifleSkeleton36.LoadPOObject('objects/karabin.po', 3.6);
  RifleSkeleton37.LoadPOObject('objects/karabin.po', 3.7);
  RifleSkeleton39.LoadPOObject('objects/karabin.po', 3.9);
  RifleSkeleton43.LoadPOObject('objects/karabin.po', 4.3);
  RifleSkeleton50.LoadPOObject('objects/karabin.po', 5.0);
  RifleSkeleton55.LoadPOObject('objects/karabin.po', 5.5);
end;

end.
