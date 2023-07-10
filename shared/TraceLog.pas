{*******************************************************}
{                                                       }
{       TraceLog Unit                                   }
{                                                       }
{       Copyright (c) 2012 Daniel Forssten              }
{                                                       }
{*******************************************************}

unit TraceLog;

interface

const
  LEVEL_OFF   = 0;
  LEVEL_DEBUG = 1;
  LEVEL_TRACE = 2;

// TODO: move log_level here

procedure Debug(const Msg: string);
procedure Trace(const Msg: string);
{$IFDEF STEAM}
procedure SteamWarning(Severity: Integer; WarnMessage: PAnsiChar); cdecl;
{$ENDIF}


implementation

uses
  // Project units
  {$IFDEF SERVER}
    Server,
  {$ELSE}
    Client,
  {$ENDIF}

  // System units
  SysUtils;


procedure Debug(const Msg: string);
begin
  if Assigned(log_level) then
    if log_level.Value >= LEVEL_DEBUG then
      WriteLn(Msg);
end;

procedure Trace(const Msg: string);
begin
  if Assigned(log_level) then
    if log_level.Value >= LEVEL_TRACE then
      WriteLn(Msg);
end;

{$IFDEF STEAM}
procedure SteamWarning(Severity: Integer; WarnMessage: PAnsiChar); cdecl;
begin
  WriteLn('[Steam] ' + WarnMessage + ' Severity:' + IntToStr(Severity));
end;
{$ENDIF}

end.
