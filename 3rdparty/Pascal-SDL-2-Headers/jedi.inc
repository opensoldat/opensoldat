{
  Simple DirectMedia Layer
  Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>

  Pascal-Header-Conversion
  Copyright (c) 2012/13 Tim Blume aka End

  sdl.inc is based on the files:
    "begin_code.h",
    "close_code.h",
	  "sdl_config.h",
	  "sdl_config_windows.h",
   	...
  it defines some variables for several Pascal-Compilers and OS-versions.

  It is based on mine updated version of jedi-sdl.inc from the SDL 1.2 Headers,
  they can be found at delphigl.com or on my github-repository:

  https://github.com/ev1313/

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
}

{
  Changelog:
  ----------
  v. 1.63-stable; 16.09.13: since GPC isn't supported anymore, i've deleted it from here, too.
  v. 1.22-alpha; 24.07.13: fixed some bugs. special thanks to kotai from pascalgamedevelopment.com
  v. 1.00-alpha; 05.07.13: Initial Alpha-Release
}

{.$define Debug}           { uncomment for debugging }

{$IFNDEF FPC}
  {$IFDEF Debug}
    {$F+,D+,Q-,L+,R+,I-,S+,Y+,A+}
  {$ELSE}
    {$F+,Q-,R-,S-,I-,A+}
  {$ENDIF}
{$ELSE}
  {$MODE DELPHI}
{$ENDIF}

{$IFDEF LINUX}
{$DEFINE UNIX}
{$ENDIF}

{$IFDEF ver70}
   {$IFDEF Windows}
     {$DEFINE Win16}
   {$ENDIF Windows}
   {$IFDEF MSDOS}
     {$DEFINE NO_EXPORTS}
   {$ENDIF MSDOS}
   {$IFDEF DPMI}
     {$DEFINE BP_DPMI}
   {$ENDIF}
   {$DEFINE OS_16_BIT}
   {$DEFINE __OS_DOS__}
{$ENDIF ver70}

{$IFDEF ver80}
   {$DEFINE Delphi}      {Delphi 1.x}
   {$DEFINE Delphi16}
   {$DEFINE Win16}
   {$DEFINE OS_16_BIT}
   {$DEFINE __OS_DOS__}
{$ENDIF ver80}

{$IFDEF ver90}
   {$DEFINE Delphi}      {Delphi 2.x}
   {$DEFINE WIN32}
   {$DEFINE WINDOWS}
{$ENDIF ver90}

{$IFDEF ver100}
   {$DEFINE Delphi}      {Delphi 3.x}
   {$DEFINE WIN32}
   {$DEFINE WINDOWS}
{$ENDIF ver100}

{$IFDEF ver93}
   {$DEFINE Delphi}      {C++ Builder 1.x}
   {$DEFINE WINDOWS}
{$ENDIF ver93}

{$IFDEF ver110}
   {$DEFINE Delphi}      {C++ Builder 3.x}
   {$DEFINE WINDOWS}
{$ENDIF ver110}

{$IFDEF ver120}
   {$DEFINE Delphi}      {Delphi 4.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Has_Int64}
   {$DEFINE WINDOWS}
{$ENDIF ver120}

{$IFDEF ver130}
   {$DEFINE Delphi}      {Delphi / C++ Builder 5.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Has_Int64}
   {$DEFINE WINDOWS}
{$ENDIF ver130}

{$IFDEF ver140}
   {$DEFINE Delphi}      {Delphi / C++ Builder 6.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Has_Int64}
   {$DEFINE HAS_TYPES}
{$ENDIF ver140}

{$IFDEF ver150}
   {$DEFINE Delphi}      {Delphi 7.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver150}

{$IFDEF ver160}
   {$DEFINE Delphi}      {Delphi 8.x}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver160}

{$IFDEF ver170}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2005}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver170}

{$IFDEF ver180}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2006 / 2007}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver180}

{$IFDEF ver185}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2007}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver185}

{$IFDEF ver190}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2007 }
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver190}

{$IFDEF ver200}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2009 }
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver200}

{$IFDEF ver210}
   {$DEFINE Delphi}      {Delphi / C++ Builder 2010}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver210}

{$IFDEF ver220}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver220}

{$IFDEF ver230}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE2}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver230}

{$IFDEF ver240}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE4}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver240}

{$IFDEF ver250}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE5}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver250}

{$IFDEF ver260}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE6}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver260}

{$IFDEF ver270}
   {$DEFINE Delphi}      {Delphi / C++ Builder XE7}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$ENDIF ver270}

{$IFNDEF FPC}	  
{$IF CompilerVersion > 27}
   {$DEFINE Delphi}      {Delphi / C++ Builder}
   {$DEFINE Delphi4UP}
   {$DEFINE Delphi5UP}
   {$DEFINE Delphi6UP}
   {$DEFINE Delphi7UP}
   {$DEFINE Delphi8UP}
   {$DEFINE Delphi9UP}
   {$DEFINE Delphi10UP}
   {$DEFINE Delphi11UP}
   {$DEFINE Delphi12UP}
   {$DEFINE Delphi13UP}
   {$DEFINE Delphi14UP}
   {$DEFINE Delphi15UP}
   {$DEFINE Delphi16UP}
   {$DEFINE Delphi17UP}
   {$WARN UNSAFE_TYPE OFF} {Disable warning for unsafe types}
   {$DEFINE Has_Int64}
   {$DEFINE Has_UInt64}
   {$DEFINE Has_Native}
   {$DEFINE HAS_TYPES}
{$IFEND}
{$ENDIF}
	  
{*************** define 16/32/64 Bit ********************}

{$IFDEF WIN16}
  {$DEFINE 16BIT}
  {$DEFINE WINDOWS}
{$ELSE}
  {$IFDEF WIN32}
    {$DEFINE 32BIT}
    {$DEFINE WINDOWS}
  {$ELSE}
    {$IFDEF WIN64}
      {$DEFINE 64BIT}
      {$DEFINE WINDOWS}
    {$ELSE}
      //TODO!!
      {$DEFINE 32BIT}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF Delphi}
  {$DEFINE USE_STDCALL}
  {$IFDEF 32Bit}
    {$DEFINE DELPHI32}
  {$ELSE}
    {$IFDEF 64Bit}
	  {$DEFINE DELPHI64}
	{$ELSE}
	  {$DEFINE DELPHI16}
	{$ENDIF}
  {$ENDIF}
  //{$ALIGN ON}
{$ENDIF Delphi}

{$IFDEF FPC}
  {$H+}
  {$PACKRECORDS C}        // Added for record
  {$PACKENUM DEFAULT}     // Added for c-like enumerators
  {$MACRO ON}             // Added For OpenGL
  {$DEFINE Delphi}
  {$DEFINE UseAT}
  {$UNDEF USE_STDCALL}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE Has_UInt64}
  {$DEFINE Has_Int64}
  {$DEFINE Has_Native}
  {$DEFINE NOCRT}
  {$IFDEF UNIX}
     {$DEFINE fpc_unix}
  {$ELSE}
     {$DEFINE __OS_DOS__}
  {$ENDIF}
  {$IFDEF WIN32}
   {$DEFINE UseWin}
  {$ENDIF}
  {$DEFINE HAS_TYPES}
{$ENDIF FPC}

{$IFDEF Win16}
  {$K+}   {smart callbacks}
{$ENDIF Win16}

{$IFDEF Win32}
  {$DEFINE OS_BigMem}
{$ENDIF Win32}

{ ************************** dos/dos-like platforms **************}
{$IFDEF Windows}
   {$DEFINE __OS_DOS__}
   {$DEFINE UseWin}
   {$DEFINE MSWINDOWS}
{$ENDIF Delphi}

{$IFDEF OS2}
   {$DEFINE __OS_DOS__}
   {$DEFINE Can_Use_DLL}
{$ENDIF Delphi}

{$IFDEF UseWin}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF Win16}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF BP_DPMI}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF USE_STDCALL}
  {$DEFINE BY_NAME}
{$ENDIF}

{*************** define LITTLE ENDIAN platforms ********************}


{$IFDEF Delphi}
  {$DEFINE IA32}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF FPC_LITTLE_ENDIAN}
    {$DEFINE IA32}
  {$ENDIF}
{$ENDIF}
