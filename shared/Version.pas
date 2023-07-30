{*************************************************************}
{                                                             }
{       Version Unit for OpenSoldat                           }
{                                                             }
{       Copyright (c) 2011      Gregor A. Cieslak             }
{       Copyright (c) 2020-2023 OpenSoldat contributors       }
{                                                             }
{*************************************************************}

unit Version;

interface

const
  OPENSOLDAT_VERSION = {$INCLUDE Version.txt};
  OPENSOLDAT_VERSION_CHARS = Length(OPENSOLDAT_VERSION);
  OPENSOLDAT_VERSION_LONG = {$INCLUDE %BUILD_ID%};
  OPENSOLDAT_VERSION_LONG_CHARS = Length(OPENSOLDAT_VERSION_LONG);
  {$IFDEF SERVER}
  DEDVERSION = '2.9.0';
  COREVERSION = '2.5';
  {$ELSE}
  DEDVERSION = 'GAME';
  {$ENDIF}
  DEMO_VERSION = $0000;  // Version number for OpenSoldat demo files


implementation

end.
