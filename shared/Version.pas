{*******************************************************}
{                                                       }
{       Version Unit                                    }
{                                                       }
{       Copyright (c) 2011 Gregor A. Cieslak            }
{                                                       }
{*******************************************************}

unit Version;

interface

const
  SOLDAT_VERSION = {$INCLUDE Version.txt};
  SOLDAT_VERSION_CHARS = Length(SOLDAT_VERSION);
  SOLDAT_VERSION_LONG = {$INCLUDE %BUILD_ID%};
  {$IFDEF SERVER}
  DEDVERSION = '2.9.0';
  COREVERSION = '2.5';
  {$ELSE}
  DEDVERSION = 'GAME';
  {$ENDIF}
  DEMO_VERSION = $0000;  // version number for soldat demo files

implementation

end.

