{******************************************************************************}
{                                                                              }
{       Vector Unit for OPENSOLDAT                                             }
{                                                                              }
{       Copyright (c) 2013 Gregor A. Cieslak                                   }
{                                                                              }
{         Modification of MyD3DX8 Unit                                         }
{           Copyright (c) 2001 by Michal Marcinkowski                          }
{                                                                              }
{******************************************************************************}
{*                                                                            *}
{*        Based upon :                                                        *}
{*          Direct3DX 8.1 Delphi adaptation by Alexey Barkovoy                *}
{*          E-Mail: directx@clootie.ru                                        *}
{*                                                                            *}
{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       d3dx8.h, d3dx8core.h, d3dx8math.h, d3dx8math.inl,             *}
{*              d3dx8effect.h, d3dx8mesh.h, d3dx8shape.h, d3dx8tex.h          *}
{*  Content:    Direct3DX 8.1 headers                                         *}
{*                                                                            *}
{*  Direct3DX 8.1 Delphi adaptation by Alexey Barkovoy                        *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 12-Feb-2005                                                     *}
{*                                                                            *}
{*  Partly based upon :                                                       *}
{*    Direct3DX 7.0 Delphi adaptation by                                      *}
{*      Arne Schäpers, e-Mail: [look at www.delphi-jedi.org/DelphiGraphics/]  *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://clootie.ru                                                       *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*  This File contains only Direct3DX 8.x Definitions.                        *}
{*  If you want to use D3DX7 version of D3DX use translation by Arne Schäpers *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit Vector;

interface

type
  PVector2 = ^TVector2;
  TVector2 = packed record
    x, y: Single;
  end;

  PVector3 = ^TVector3;
  TVector3 = packed record
    x, y, z: Single;
  end;

function Vector2(x, y: Single): TVector2;

function Vec2Length(v: TVector2): Single;
function Vec2Length2(v: TVector2): Single;
function Vec2Dot(v1, v2: TVector2): Single;
function Vec2Add(v1, v2: TVector2): TVector2;
function Vec2Subtract(v1, v2: TVector2): TVector2;
function Vec2Scale(out vOut: TVector2; v: TVector2; s: Single): PVector2;
function Vec2Normalize(out vOut: TVector2; v: TVector2): PVector2;

function Vec3Length(v: TVector3): Single;

implementation

function Vector2(x, y: Single): TVector2;
begin
  Result.x := x;
  Result.y := y;
end;

function Vec2Length(v: TVector2): Single;
begin
  with v do
    Result := Sqrt(Sqr(x) + Sqr(y));
end;

function Vec2Length2(v: TVector2): Single;
begin
  with v do
    Result := Sqr(x) + Sqr(y);
end;

function Vec2Dot(v1, v2: TVector2): Single;
begin
  Result := v1.x * v2.x + v1.y * v2.y;;
end;

function Vec2Add(v1, v2: TVector2): TVector2;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
end;

function Vec2Subtract(v1, v2: TVector2): TVector2;
begin
  Result.x := v1.x - v2.x;
  Result.y := v1.y - v2.y;
end;

function Vec2Scale(out vOut: TVector2; v: TVector2; s: Single): PVector2;
begin
  vOut.x := v.x * s;
  vOut.y := v.y * s;
  Result := @vOut;
end;

function Vec2Normalize(out vOut: TVector2; v: TVector2): PVector2;
var
  len: Single;
begin
  len := Vec2Length(v);
  if (len < 0.001) and (len > -0.001) then
  begin
    vOut.x := 0;
    vOut.y := 0;
  end else
  begin
    vOut.x := v.x / len;
    vOut.y := v.y / len;
  end;
  Result := @vOut;
end;

function Vec3Length(v: TVector3): Single;
begin
  with v do
    Result := Sqrt(Sqr(x) + Sqr(y) + Sqr(z));
end;

end.
