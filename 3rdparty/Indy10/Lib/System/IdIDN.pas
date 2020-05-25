unit IdIDN;
{
  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2012, Chad Z. Hower and the Indy Pit Crew. All rights reserved.

Original Author:  J. Peter Mugaas

This file uses the "Windows Microsoft Internationalized Domain Names (IDN) Mitigation APIs 1.1"

There is a download for some Windows versions at:

http://www.microsoft.com/en-us/download/details.aspx?id=734

for Windows XP and that SDK includes a package of run-time libraries that might
need to be redistributed to Windows XP users.

On some later Windows versions, this is redistributable is not needed.

For Windows 8, we do not use this.

From: http://msdn.microsoft.com/en-us/library/windows/desktop/ms738520%28v=vs.85%29.aspx

"On Windows 8 Consumer Preview and Windows Server "8" Beta, the getaddrinfo
function provides support for IRI or Internationalized Domain Name (IDN) parsing
applied to the name passed in the pNodeName parameter. Winsock performs
Punycode/IDN encoding and conversion. This behavior can be disabled using the
AI_DISABLE_IDN_ENCODING flag discussed below.

   }
interface

{$I IdCompilerDefines.inc}

uses
  IdGlobal
  {$IFDEF WIN32_OR_WIN64}
  , Windows
  {$ENDIF}
  ;

{$IFDEF WIN32_OR_WIN64}

{

}
// ==++==
//
// Copyright (c) Microsoft Corporation.  All Rights Reserved
//
// ==++==

// IdnDl.h
//
// WARNING: This .DLL is downlevel only.
//
// This file contains the downlevel versions of the scripts APIs
//
// 06 Jun 2005     Shawn Steele    Initial Implementation
const
  {$EXTERNALSYM VS_ALLOW_LATIN}
  VS_ALLOW_LATIN              = $0001;
  {$EXTERNALSYM GSS_ALLOW_INHERITED_COMMON}
  GSS_ALLOW_INHERITED_COMMON  = $0001;

type
  {$EXTERNALSYM DownlevelGetLocaleScripts_LPFN}
  DownlevelGetLocaleScripts_LPFN = function (
     lpLocaleName : LPCWSTR;   // Locale Name
     lpScripts : LPWSTR;       // Output buffer for scripts
     cchScripts : Integer      // size of output buffer
  ) : Integer stdcall;
  {$EXTERNALSYM DownlevelGetStringScripts_LPFN}
  DownlevelGetStringScripts_LPFN = function (
     dwFlags : DWORD;          // optional behavior flags
     lpString : LPCWSTR;       // Unicode character input string
     cchString : Integer;      // size of input string
     lpScripts : LPWSTR;       // Script list output string
     cchScripts : Integer      // size of output string
  ) : Integer stdcall;
  {$EXTERNALSYM DownlevelVerifyScripts_LPFN}
  DownlevelVerifyScripts_LPFN = function (
     dwFlags : DWORD;            // optional behavior flags
     lpLocaleScripts : LPCWSTR;  // Locale list of scripts string
     cchLocaleScripts : Integer; // size of locale script list string
     lpTestScripts : LPCWSTR;    // test scripts string
     cchTestScripts : Integer    // size of test list string
  ) : BOOL stdcall;

// Normalization.h
// Copyright 2002 Microsoft
//
// Excerpted from LH winnls.h
type
  {$EXTERNALSYM NORM_FORM}
  NORM_FORM = DWORD;

const
  {$EXTERNALSYM NormalizationOther}
   NormalizationOther  = 0;       // Not supported
  {$EXTERNALSYM NormalizationC}
   NormalizationC     = $1;       // Each base plus combining characters to the canonical precomposed equivalent.
  {$EXTERNALSYM NormalizationD}
   NormalizationD     = $2;       // Each precomposed character to its canonical decomposed equivalent.
   {$EXTERNALSYM NormalizationKC}
   NormalizationKC    = $5;       // Each base plus combining characters to the canonical precomposed
                                  //   equivalents and all compatibility characters to their equivalents.
   {$EXTERNALSYM NormalizationKD}
   NormalizationKD    = $6;       // Each precomposed character to its canonical decomposed equivalent
                                  //   and all compatibility characters to their equivalents.

//
// IDN (International Domain Name) Flags
//
const
  {$EXTERNALSYM IDN_ALLOW_UNASSIGNED}
   IDN_ALLOW_UNASSIGNED     = $01;  // Allow unassigned "query" behavior per RFC 3454
  {$EXTERNALSYM IDN_USE_STD3_ASCII_RULES}
   IDN_USE_STD3_ASCII_RULES  = $02;  // Enforce STD3 ASCII restrictions for legal characters

type
//
// Windows API Normalization Functions
//
  {$EXTERNALSYM NormalizeString_LPFN}
  NormalizeString_LPFN = function ( NormForm : NORM_FORM;
    lpString : LPCWSTR; cwLength : Integer) : DWORD stdcall;
  {$EXTERNALSYM IsNormalizedString_LPFN}
  IsNormalizedString_LPFN = function (  NormForm : NORM_FORM;
    lpString : LPCWSTR; cwLength : Integer ) : BOOL stdcall;
//
// IDN (International Domain Name) Functions
//
  {$EXTERNALSYM IdnToAscii_LPFN}
  IdnToAscii_LPFN = function(dwFlags : DWORD;
    lpUnicodeCharStr : LPCWSTR;
    cchUnicodeChar : Integer;
    lpNameprepCharStr : LPWSTR;
    cchNameprepChar : Integer  ) : Integer stdcall;
  {$EXTERNALSYM IdnToNameprepUnicode_LPFN}
  IdnToNameprepUnicode_LPFN = function (dwFlags : DWORd;
    lpUnicodeCharStr : LPCWSTR;
    cchUnicodeChar : Integer;
    lpASCIICharStr : LPWSTR;
    cchASCIIChar : Integer) : Integer stdcall;
  {$EXTERNALSYM IdnToUnicode_LPFN}
  IdnToUnicode_LPFN = function (dwFlags : DWORD;
    lpASCIICharSt : LPCWSTR;
    cchASCIIChar : Integer;
    lpUnicodeCharStr : LPWSTR;
    cchUnicodeChar : Integer) : Integer stdcall;

var
  {$EXTERNALSYM DownlevelGetLocaleScripts}
  DownlevelGetLocaleScripts : DownlevelGetLocaleScripts_LPFN = nil;
  {$EXTERNALSYM DownlevelGetStringScripts}
  DownlevelGetStringScripts : DownlevelGetStringScripts_LPFN = nil;
  {$EXTERNALSYM DownlevelVerifyScripts}
  DownlevelVerifyScripts : DownlevelVerifyScripts_LPFN = nil;

  {$EXTERNALSYM IsNormalizedString}
  IsNormalizedString : IsNormalizedString_LPFN = nil;
  {$EXTERNALSYM NormalizeString}
  NormalizeString : NormalizeString_LPFN = nil;
  {$EXTERNALSYM IdnToUnicode}
  IdnToUnicode : IdnToUnicode_LPFN = nil;
  {$EXTERNALSYM IdnToNameprepUnicode}
  IdnToNameprepUnicode : IdnToNameprepUnicode_LPFN = nil;
  {$EXTERNALSYM IdnToAscii}
  IdnToAscii : IdnToAscii_LPFN = nil;

const
  LibNDL = 'IdnDL.dll';
  LibNormaliz = 'Normaliz.dll';

  fn_DownlevelGetLocaleScripts = 'DownlevelGetLocaleScripts';
  fn_DownlevelGetStringScripts = 'DownlevelGetStringScripts';
  fn_DownlevelVerifyScripts = 'DownlevelVerifyScripts';

  fn_IsNormalizedString = 'IsNormalizedString';
  fn_NormalizeString = 'NormalizeString';
  fn_IdnToUnicode = 'IdnToUnicode';
  fn_IdnToNameprepUnicode = 'IdnToNameprepUnicode';
  fn_IdnToAscii = 'IdnToAscii';

{$ENDIF} // {$IFDEF WIN32_OR_WIN64}

function UseIDNAPI : Boolean;
function IDNToPunnyCode(const AIDN : TIdUnicodeString) : String;
function PunnyCodeToIDN(const APunnyCode : String) : TIdUnicodeString;

procedure InitIDNLibrary;
procedure CloseIDNLibrary;

implementation

{$IFDEF WIN32_OR_WIN64}

uses
  SysUtils;

var
  hIdnDL : THandle = 0;
  hNormaliz : THandle = 0;

function UseIDNAPI : Boolean;
begin
  Result := not IndyCheckWindowsVersion(6, 2);
  if Result then begin
    Result := Assigned( IdnToAscii ) and Assigned( IdnToUnicode );
  end;
end;

function PunnyCodeToIDN(const APunnyCode : String) : TIdUnicodeString;
var
  {$IFNDEF STRING_IS_UNICODE}
  LTemp: TIdUnicodeString;
  {$ENDIF}
  LIDN : TIdUnicodeString;
  Len : Integer;
begin
  Result := '';
  if Assigned(IdnToUnicode) then
  begin
    {$IFNDEF STRING_IS_UNICODE}
    LTemp := TIdUnicodeString(APunnyCode); // explicit convert to Unicode
    {$ENDIF}
    Len := IdnToUnicode(0,
      {$IFDEF STRING_IS_UNICODE}
      PIdWideChar(APunnyCode), Length(APunnyCode)
      {$ELSE}
      PIdWideChar(LTemp), Length(LTemp)
      {$ENDIF},
      nil, 0);
    if Len = 0 then begin
      IndyRaiseLastError;
    end;
    SetLength(LIDN, Len);
    Len := IdnToUnicode(0,
      {$IFDEF STRING_IS_UNICODE}
      PIdWideChar(APunnyCode), Length(APunnyCode)
      {$ELSE}
      PIdWideChar(LTemp), Length(LTemp)
      {$ENDIF},
      PIdWideChar(LIDN), Len);
    if Len = 0 then begin
      IndyRaiseLastError;
    end;
    Result := LIDN;
  end else begin
    // TODO: manual implementation here ...
  end;
end;

function IDNToPunnyCode(const AIDN : TIdUnicodeString) : String;
var
  LPunnyCode : TIdUnicodeString;
  Len : Integer;
begin
  Result := '';
  if Assigned(IdnToAscii) then
  begin
    Len := IdnToAscii(0, PIdWideChar(AIDN), Length(AIDN), nil, 0);
    if Len = 0 then begin
      IndyRaiseLastError;
    end;
    SetLength(LPunnyCode, Len);
    Len := IdnToAscii(0, PIdWideChar(AIDN), Length(AIDN), PIdWideChar(LPunnyCode), Len);
    if Len = 0 then begin
      IndyRaiseLastError;
    end;
    {$IFDEF STRING_IS_ANSI}
    Result := AnsiString(LPunnyCode); // explicit convert to Ansi (no data loss because content is ASCII)
    {$ELSE}
    Result := LPunnyCode;
    {$ENDIF}
  end else
  begin
    // TODO: manual implementation here ...
  end;
end;

procedure InitIDNLibrary;
begin
  if hIdnDL = 0 then
  begin
    hIdnDL := SafeLoadLibrary(LibNDL);
    if hIdnDL <> 0 then
    begin
      DownlevelGetLocaleScripts := GetProcAddress(hIdnDL, fn_DownlevelGetLocaleScripts);
      DownlevelGetStringScripts := GetProcAddress(hIdnDL, fn_DownlevelGetStringScripts);
      DownlevelVerifyScripts := GetProcAddress(hIdnDL, fn_DownlevelVerifyScripts);
    end;
  end;

  if hNormaliz = 0 then
  begin
    hNormaliz := SafeLoadLibrary(LibNormaliz);
    if hNormaliz <> 0 then
    begin
      IdnToUnicode := GetProcAddress(hNormaliz, fn_IdnToUnicode);
      IdnToNameprepUnicode := GetProcAddress(hNormaliz, fn_IdnToNameprepUnicode);
      IdnToAscii := GetProcAddress(hNormaliz, fn_IdnToAscii);
      IsNormalizedString := GetProcAddress(hNormaliz,fn_IsNormalizedString);
      NormalizeString := GetProcAddress(hNormaliz, fn_NormalizeString);
    end;
  end;
end;

procedure CloseIDNLibrary;
var
  h : THandle;
begin
  h := InterlockedExchangeTHandle(hIdnDL, 0);
  if h <> 0 then begin
    FreeLibrary(h);
  end;

  h := InterlockedExchangeTHandle(hNormaliz, 0);
  if h <> 0 then begin
    FreeLibrary(h);
  end;

  IsNormalizedString := nil;
  NormalizeString := nil;

  IdnToUnicode := nil;
  IdnToNameprepUnicode := nil;
  IdnToAscii := nil;
end;

{$ELSE}

function UseIDNAPI : Boolean;
begin
  Result := False;
end;

function IDNToPunnyCode(const AIDN : TIdUnicodeString) : String;
begin
  Todo('IDNToPunnyCode() is not implemented for this platform');
end;

function PunnyCodeToIDN(const APunnyCode : String) : TIdUnicodeString;
begin
  Todo('PunnyCodeToIDN() is not implemented for this platform');
end;

procedure InitIDNLibrary;
begin
end;

procedure CloseIDNLibrary;
begin
end;

{$ENDIF} // {$IFDEF WIN32_OR_WIN64}

initialization
finalization
  CloseIDNLibrary;

end.
