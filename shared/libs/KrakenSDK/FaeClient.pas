{*********************************************************}
{                                                         }
{                  Fae/NanoKraken SDK                     }
{            Copyright 2018 Oliver Kuckertz               }
{                                                         }
{*********************************************************}

unit FaeClient;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

uses
  // Library units
  FaeBase;


const
{$IFDEF MSWINDOWS}
  FAE_MODULE_NAME = 'fae.dll';
{$ELSE}
  {$IFDEF DARWIN}
  FAE_MODULE_NAME = 'libfae.dylib';
  {$ELSE}
  FAE_MODULE_NAME = 'libfae.so';
  {$ENDIF}
{$ENDIF}

  // Flags for FaeAuthSubmit
  FAE_AUTH_DERIVE_KEY = 1;

// ----------------------------------------------------------------------------
// Client (fae)
// ----------------------------------------------------------------------------

  function FaeIsEnabled(): Boolean;
  cdecl; external FAE_MODULE_NAME name 'FaeIsEnabled';

  // call on startup
  procedure FaePreflight();
  cdecl; external FAE_MODULE_NAME name 'Fae0';

  // call this from the game loop -- does nothing unless dynamic validation
  // scripts are active, which will then validate the game loop's state
  procedure FaeOnTick();
  cdecl; external FAE_MODULE_NAME name 'Fae1';

  // synchronously answers a challenge (may take a few ms)
  procedure FaeAuthSync(Challenge: PFaeChallenge; out Response:
    TFaeResponseBox; DerivedKey: PFaeKey);
  cdecl; external FAE_MODULE_NAME name 'Fae2';

  // returns a handle to pass to FaeAuthFetch or FaeAuthCancel
  function FaeAuthSubmit(Challenge: PFaeChallenge; Flags: UInt32): Pointer;
  cdecl; external FAE_MODULE_NAME name 'Fae3';

  // returns true iff. a response was written and auth was invalidated
  function FaeAuthFetch(Auth: Pointer; out Response: TFaeResponseBox;
    DerivedKey: PFaeKey): Boolean;
  cdecl; external FAE_MODULE_NAME name 'Fae4';

  // waits for auth op to complete (may take a few ms) and invalidates auth
  procedure FaeAuthCancel(Auth: Pointer);
  cdecl; external FAE_MODULE_NAME name 'Fae5';


implementation

end.
