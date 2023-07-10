{*********************************************************}
{                                                         }
{                  Fae/NanoKraken SDK                     }
{            Copyright 2018 Oliver Kuckertz               }
{                                                         }
{*********************************************************}

unit FaeBase;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

// ----------------------------------------------------------------------------
// Common
// ----------------------------------------------------------------------------

type
  PFaeSecret = ^TFaeSecret;
  TFaeSecret = array[1..32] of Byte;

  PFaeKey = ^TFaeKey;
  TFaeKey = array[1..32] of Byte;

  PFaeChallenge = ^TFaeChallenge;
  TFaeChallenge = packed record
    EphKey:          array[1..32] of Byte;
    EncryptedSecret: TFaeSecret;
  end;

  PFaeResponse = ^TFaeResponse;
  TFaeResponse = packed record
    GameKey:     array[1..32] of Byte;
    GameVersion: array[1..32] of Char;
    ValidUntil:  Int64;    // LE unix timestamp
    FaeBuild:    Cardinal; // LE
    Status:      Cardinal; // LE
    StatusStr:   array[1..32] of Char;
  end;

  PFaeResponseBox = ^TFaeResponseBox;
  TFaeResponseBox = packed record
    OuterStatus: Cardinal;
    Reserved:    Cardinal;
    MAC:         array[1..16] of Byte;
    CipherText:  array[1..sizeof(TFaeResponse)] of Byte;
  end;


implementation

end.
