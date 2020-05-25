{*********************************************************}
{                                                         }
{                  Fae/NanoKraken SDK                     }
{            Copyright 2018 Oliver Kuckertz               }
{                                                         }
{*********************************************************}

unit FaeRemoteAttestation;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses FaeBase;

const
{$IFDEF MSWINDOWS}
  FAECHECK_MODULE_NAME = 'faecheck.dll';
{$ELSE}
  {$IFDEF DARWIN}
  FAECHECK_MODULE_NAME = 'libfaecheck.dylib';
  {$ELSE}
  FAECHECK_MODULE_NAME = 'libfaecheck.so';
  {$ENDIF}
{$ENDIF}

  FAECHECK_OK = 0;
  FAECHECK_ERR_INVALID = 1;
  FAECHECK_ERR_CLOCK = 2;

type
  PFaeMac = ^TFaeMac;
  TFaeMac = array[1..16] of Byte;

  PFaeNonce = ^TFaeNonce;
  TFaeNonce = array[1..24] of Byte;

// ----------------------------------------------------------------------------
// Remote Attestation (faecheck)
// ----------------------------------------------------------------------------

  procedure FaeInitSecret(Secret: PFaeSecret);
  cdecl; external FAECHECK_MODULE_NAME name 'FaeInitSecret';

  procedure FaeInitChallenge(Secret: PFaeSecret; out Challenge: TFaeChallenge);
  cdecl; external FAECHECK_MODULE_NAME name 'FaeInitChallenge';

  procedure FaeDeriveKey(Secret: PFaeSecret; Result: PFaeKey);
  cdecl; external FAECHECK_MODULE_NAME name 'FaeDeriveKey';

  function FaeCheck(Secret: PFaeSecret; Box: PFaeResponseBox; out Result:
    TFaeResponse): Integer;
  cdecl; external FAECHECK_MODULE_NAME name 'FaeCheck';

  procedure FaeLock(Mac: PFaeMac; CipherText: Pointer; Key: PFaeKey; Nonce:
    PFaeNonce; AdditionalData: Pointer; AdSize: NativeUint; PlainText: Pointer;
    Size: NativeUint);
  cdecl; external FAECHECK_MODULE_NAME name 'FaeLock';

  function FaeUnlock(PlainText: Pointer; Key: PFaeKey; Nonce:
    PFaeNonce; Mac: PFaeMac; AdditionalData: Pointer; AdSize: NativeUint;
    CipherText: Pointer; TextSize: NativeUint): Boolean;
  cdecl; external FAECHECK_MODULE_NAME name 'FaeUnlock';

implementation

end.
