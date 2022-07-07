{*******************************************************}
{                                                       }
{       Particle Unit for OPENSOLDAT                    }
{                                                       }
{       Copyright (c) 2001-02 Michal Marcinkowski       }
{                                                       }
{ PARTS ver. 1.0.7                                      }
{ PARTICLE & CONSTRAINT PHYSICS MODULE FOR DELPHI 2D    }
{  by Michal Marcinkowski                               }
{                                                       }
{  version history:                                     }
{  Current - Modified for OPENSOLDAT                    }
{  1.0.7 - Changed to 2D                                }
{  1.0.6 - Added Constraint Active variable             }
{  1.0.5 - Added PO Constraints only loading            }
{  1.0.4 - Changed to D3DX Vectors                      }
{  1.0.3 - Added PO loader scaling                      }
{  1.0.2 - Added Euler Integrator                       }
{  1.0.1 - Added PO files loading                       }
{                                                       }
{*******************************************************}

unit Parts;

interface

uses
  Vector;

const
  NUM_PARTICLES = 560;
  RKV = 0.98;

type
  Constraint = record
    Active: Boolean;
    PartA, PartB: Integer;
    Restlength: Single;
  end;

  ParticleSystem = object
    Active: array[1..NUM_PARTICLES] of Boolean;
    Pos: array[1..NUM_PARTICLES] of TVector2;
    Velocity: array[1..NUM_PARTICLES] of TVector2;
    OldPos: array[1..NUM_PARTICLES] of TVector2;
    Forces: array[1..NUM_PARTICLES] of TVector2;
    OneOverMass: array[1..NUM_PARTICLES] of Single;
    TimeStep: Single;
    Gravity, VDamping, EDamping: Single;
    ConstraintCount: Integer;
    PartCount: Integer;
    Constraints: array[1..NUM_PARTICLES] of Constraint;
  public
    procedure DoVerletTimeStep;
    procedure DoVerletTimeStepFor(I, J: Integer);
    procedure DoEulerTimeStep;
    procedure DoEulerTimeStepFor(I: Integer);
    procedure CreatePart(Start, Vel: TVector2; Mass: Single; Num: Integer);
    procedure MakeConstraint(PA, PB: Integer; Rest: Single);
    procedure Clone(Other: ParticleSystem);
    procedure LoadPOObject(Filename: string; Scale: Single);
    procedure StopAllParts;
    procedure Destroy;
    procedure SatisfyConstraints;
  private
    procedure Verlet(I: Integer);
    procedure Euler(I: Integer);
    procedure SatisfyConstraintsFor(I: Integer);
  end;

implementation

uses
  SysUtils, PhysFS;

procedure ParticleSystem.DoVerletTimeStep;
var
  I: Integer;
begin
  for I := 1 to NUM_PARTICLES do
    if Active[I] then
      Verlet(I);
  SatisfyConstraints;
end;

procedure ParticleSystem.DoVerletTimeStepFor(I, J: Integer);
begin
  Verlet(I);
  SatisfyConstraintsFor(J);
end;

procedure ParticleSystem.DoEulerTimeStepFor(I: Integer);
begin
  Euler(I);
end;

procedure ParticleSystem.DoEulerTimeStep;
var
  I: Integer;
begin
  for I := 1 to NUM_PARTICLES do
    if Active[I] then
      Euler(I);
end;

procedure ParticleSystem.Euler(I: Integer);
var
  TempPos, S: TVector2;
begin
  // Accumulate Forces
  Forces[I].Y := Forces[I].Y + Gravity;
  TempPos := Pos[I];

  Vec2Scale(S, Forces[I], OneOverMass[I]);
  Vec2Scale(S, S, Sqr(TimeStep));

  Velocity[I] := Vec2Add(Velocity[I], S);
  Pos[I] := Vec2Add(Pos[I], Velocity[I]);
  Vec2Scale(Velocity[I], Velocity[I], EDamping);
  OldPos[I] := TempPos;

  Forces[I].X := 0;
  Forces[I].Y := 0;
end;

procedure ParticleSystem.Verlet(I: Integer);
var
  TempPos, S1, S2, D: TVector2;
begin
  // Accumulate Forces
  Forces[I].Y := Forces[I].Y + Gravity;
  TempPos := Pos[I];

  // Pos[I]:= 2 * Pos[I] - OldPos[I] + Forces[I]{ / Mass} * TimeStep * TimeStep;  {Verlet integration}
  Vec2Scale(S1, Pos[I], 1.0 + VDamping);
  Vec2Scale(S2, OldPos[I], VDamping);

  D := Vec2Subtract(S1, S2);
  Vec2Scale(S1, Forces[I], OneOverMass[I]);
  Vec2Scale(S2, S1, Sqr(TimeStep));

  Pos[I] := Vec2Add(D, S2);
  OldPos[I] := TempPos;

  Forces[I].X := 0;
  Forces[I].Y := 0;
end;

procedure ParticleSystem.SatisfyConstraints;
var
  I: Integer;
  Delta, D: TVector2;
  Deltalength, Diff: Single;
begin
  if ConstraintCount > 0 then
    for I := 1 to ConstraintCount do
      with Constraints[I] do
        if Active then
        begin
          Diff := 0;
          Delta := Vec2Subtract(Pos[PartB], Pos[PartA]);
          Deltalength := Sqrt(Vec2Dot(Delta, Delta));
          if Deltalength <> 0 then
            Diff := (Deltalength - Restlength) / Deltalength;
          if OneOverMass[PartA] > 0 then
          begin
            Vec2Scale(D, Delta, 0.5 * Diff);
            Pos[PartA] := Vec2Add(Pos[PartA], D);
          end;
          if OneOverMass[PartB] > 0 then
          begin
            Vec2Scale(D, Delta, 0.5 * Diff);
            Pos[PartB] := Vec2Subtract(Pos[PartB], D);
          end;
        end;
end;

procedure ParticleSystem.SatisfyConstraintsFor(I: Integer);
var
  Delta, D: TVector2;
  Deltalength, Diff: Single;
begin
  with Constraints[i] do
  begin
    Diff := 0;
    Delta := Vec2Subtract(Pos[PartB], Pos[PartA]);
    Deltalength := Sqrt(Vec2Dot(Delta, Delta));
    if Deltalength <> 0 then
      Diff := (deltalength - Restlength) / Deltalength;
    if OneOverMass[PartA] > 0 then
    begin
      Vec2Scale(D, Delta, 0.5 * Diff);
      Pos[PartA] := Vec2Add(Pos[PartA], D);
    end;
    if OneOverMass[PartB] > 0 then
    begin
      Vec2Scale(D, Delta, 0.5 * Diff);
      Pos[PartB] := Vec2Subtract(Pos[PartB], D);
    end;
  end;
end;

procedure ParticleSystem.CreatePart(Start, Vel: TVector2; Mass: Single; Num: Integer);
begin
  // Num is now the active Part
  Active[Num] := True;
  Pos[Num] := Start;
  Velocity[Num] := Vel;

  OldPos[Num] := Start;
  OneOverMass[Num] := 1 / Mass;
end;

procedure ParticleSystem.MakeConstraint(PA, PB: Integer; Rest: Single);
begin
  Inc(ConstraintCount);
  with Constraints[ConstraintCount] do
  begin
    Active := True;
    PartA := PA;
    PartB := PB;
    Restlength := Rest;
  end;
end;

procedure ParticleSystem.Clone(Other: ParticleSystem);
var
  I: Integer;
  OtherConstraint: ^Constraint;
begin
  ConstraintCount := Other.ConstraintCount;
  PartCount := Other.PartCount;

  Move(Other.Active,      Active,      PartCount * SizeOf(Active[1]));
  Move(Other.Pos,         Pos,         PartCount * SizeOf(Pos[1]));
  Move(Other.Velocity,    Velocity,    PartCount * SizeOf(Velocity[1]));
  Move(Other.OldPos,      OldPos,      PartCount * SizeOf(OldPos[1]));
  Move(Other.OneOverMass, OneOverMass, PartCount * SizeOf(OneOverMass[1]));

  for I := 1 to ConstraintCount do
  begin
    OtherConstraint := @Other.Constraints[I];
    with Constraints[I] do
    begin
      Active := OtherConstraint.Active;
      PartA := OtherConstraint.PartA;
      PartB := OtherConstraint.PartB;
      Restlength := OtherConstraint.Restlength;
    end;
  end;
end;

procedure ParticleSystem.LoadPOObject(Filename: string; Scale: Single);
var
  F: PHYSFS_File;
  Nm: string = '';
  X: string = '';
  Y: string = '';
  Z: string = '';
  A: string = '';
  B: string = '';
  P, Delta, V: TVector2;
  Pa, Pb: Integer;
  I: Integer;
begin
  if not PHYSFS_Exists(PChar(Filename)) then
    Exit;
  V.X := 0;
  V.Y := 0;
  I := 0;
  ConstraintCount := 0;

  {$I-}
  F := PHYSFS_openRead(PChar(Filename));

  if IOResult <> 0 then
    Exit;

  repeat
    PhysFS_ReadLN(F, Nm);  // name
    if Nm <> 'CONSTRAINTS' then
    begin
      PhysFS_ReadLN(F, X);  // X
      PhysFS_ReadLN(F, Y);  // Y
      PhysFS_ReadLN(F, Z);  // Z

      // make object
      P.X := -StrToFloat(X) * Scale / 1.2;
      P.Y := -StrToFloat(Z) * Scale;

      Inc(I);
      CreatePart(P, V, 1, I);
    end;
  until Nm = 'CONSTRAINTS';

  PartCount := I;

  repeat  // CONSTRAINTS
    PhysFS_ReadLN(F, A);  // Part A
    if A = 'ENDFILE' then
      Break;

    PhysFS_ReadLN(F, B);  // Part B
    {if not Eof(F) then
    begin}
    Delete(A, 1, 1);
    Delete(B, 1, 1);
    Pa := StrToInt(A);
    Pb := StrToInt(B);

    Delta := Vec2Subtract(Pos[Pa], Pos[Pb]);
    MakeConstraint(Pa, Pb, Sqrt(Vec2Dot(Delta, Delta)));
    {end;}
  until A = 'ENDFILE';

  PHYSFS_close(F);
  {$I+}
end;

procedure ParticleSystem.StopAllParts;
var
  I: Integer;
begin
  for I := 1 to NUM_PARTICLES do
    if Active[I] then
    begin
      Velocity[I].X := 0;
      Velocity[I].Y := 0;
      OldPos[I] := Pos[I];
    end;
end;

procedure ParticleSystem.Destroy;
var
  I: Integer;
begin
  for I := 1 to NUM_PARTICLES do
  begin
    Active[I] := False;
    Pos[I].X := 0;
    Pos[I].Y := 0;
    OldPos[I] := Pos[i];
    Velocity[I].X := 0;
    Velocity[I].Y := 0;
    Forces[I].X := 0;
    Forces[I].Y := 0;
  end;
  ConstraintCount := 0;
end;

end.
