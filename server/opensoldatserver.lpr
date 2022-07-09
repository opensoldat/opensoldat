{*********************************************************}
{                                                         }
{   OpenSoldatServer                                      }
{                                                         }
{   Copyright (c) 2001 Michal Marcinkowski                }
{                                                         }
{*********************************************************}

program opensoldatserver;

{$IFDEF DARWIN}
{$linklib physfs}
{$linklib GameNetworkingSockets}
{$ENDIF}

uses
  {$IFNDEF WINDOWS}
  cthreads, // needs to be first included unit in project
  {$ENDIF}
  {$IFDEF AUTOUPDATER}
  AutoUpdater,
  {$ENDIF}
  Main in 'Main.pas';

{$IFDEF MSWINDOWS}
const
  {$IFNDEF DEBUG}
    IMAGE_DLLCHARACTERISTICS_NX_COMPAT = $0100;
    {$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_NX_COMPAT}

    IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE = $0040;
    {$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE}

    {$IFDEF CPU64}
      IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA = $0020;
      {$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA}
    {$ENDIF}
  {$ENDIF}
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;
  {$SetPEOptFlags IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE}
{$ENDIF MSWINDOWS}

begin
  {$IFDEF AUTOUPDATER}
  StartAutoUpdater;
  {$ENDIF}
  RunServer;

  DefaultSystemCodePage := CP_UTF8;
end.
