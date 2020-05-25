{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.54    2/9/2005 8:45:38 PM  JPMugaas
  Should work.

  Rev 1.53    2/8/05 6:37:38 PM  RLebeau
  Added default value to ASize parameter of ReadStringFromStream()

  Rev 1.52    2/8/05 5:57:10 PM  RLebeau
  added AppendString(), CopyTIdLongWord(), and CopyTIdString() functions

  Rev 1.51    1/31/05 6:01:40 PM  RLebeau
  Renamed GetCurrentThreadHandle() to CurrentThreadId() and changed the return
  type from THandle to to TIdPID.

  Reworked conditionals for SetThreadName() and updated the implementation to
  support naming threads under DotNet.

  Rev 1.50    1/27/05 3:40:04 PM  RLebeau
  Updated BytesToShort() to actually use the AIndex parameter that was added
  earlier.

  Rev 1.49    1/24/2005 7:35:36 PM  JPMugaas
  Foxed ma,e om CopyTIdIPV6Address/

  Rev 1.48    1/17/2005 7:26:44 PM  JPMugaas
  Made an IPv6 address byte copy function.

  Rev 1.47    1/15/2005 6:01:38 PM  JPMugaas
  Removed some new procedures for extracting  int values from a TIdBytes and
  made some other procedures have an optional index paramter.

  Rev 1.46    1/13/05 11:11:20 AM  RLebeau
  Changed BytesToRaw() to pass TIdBytes by 'const' rather than by 'var'

  Rev 1.45    1/8/2005 3:56:58 PM  JPMugaas
  Added routiens for copying integer values to and from TIdBytes.  These are
  useful for some protocols.

  Rev 1.44    24/11/2004 16:26:24  ANeillans
  GetTickCount corrected, as per Paul Cooper's post in
  atozedsoftware.indy.general.

  Rev 1.43    11/13/04 10:47:28 PM  RLebeau
  Fixed compiler errors

  Rev 1.42    11/12/04 1:02:42 PM  RLebeau
  Added RawToBytesF() and BytesToRaw() functions

  Added asserts to BytesTo...() functions

  Rev 1.41    10/26/2004 8:20:02 PM  JPMugaas
  Fixed some oversights with conversion.  OOPS!!!

  Rev 1.40    10/26/2004 8:00:54 PM  JPMugaas
  Now uses TIdStrings for DotNET portability.

  Rev 1.39    2004.10.26 7:35:16 PM  czhower
  Moved IndyCat to CType in IdBaseComponent

  Rev 1.38    24/10/2004 21:29:52  ANeillans
  Corrected error in GetTickCount,
  was Result := Trunc(nTime / (Freq * 1000))
  should be Result := Trunc((nTime / Freq) * 1000)

  Rev 1.37    20/10/2004 01:08:20  CCostelloe
  Bug fix

  Rev 1.36    28.09.2004 20:36:58  Andreas Hausladen
  Works now with Delphi 5

    Rev 1.35    9/23/2004 11:36:04 PM  DSiders
  Modified Ticks function (Win32) to correct RangeOverflow error.  (Reported by
  Mike Potter)

  Rev 1.34    24.09.2004 02:16:04  Andreas Hausladen
  Added ReadTIdBytesFromStream and ReadCharFromStream function to supress .NET
  warnings.

  Rev 1.33    9/5/2004 2:55:00 AM  JPMugaas
  function BytesToWord(const AValue: TIdBytes): Word; was not listed in the
  interface.

  Rev 1.32    04.09.2004 17:12:56  Andreas Hausladen
  New PosIdx function (without pointers)

  Rev 1.31    27.08.2004 22:02:20  Andreas Hausladen
  Speed optimization ("const" for string parameters)
  rewritten PosIdx function with AStartPos = 0 handling
  new ToArrayF() functions (faster in native code because the TIdBytes array
  must have the required len before the ToArrayF function is called)

  Rev 1.30    24.08.2004 19:48:28  Andreas Hausladen
  Some optimizations
  Removed IFDEF for IdDelete and IdInsert

  Rev 1.29    8/17/2004 2:54:08 PM  JPMugaas
  Fix compiler warning about widening operends.  Int64 can sometimes incur a
  performance penalty.

  Rev 1.28    8/15/04 5:57:06 PM  RLebeau
  Tweaks to PosIdx()

  Rev 1.27    7/23/04 10:13:16 PM  RLebeau
  Updated ReadStringFromStream() to resize the result using the actual number
  of bytes read from the stream

    Rev 1.26    7/18/2004 2:45:38 PM  DSiders
  Added localization comments.

  Rev 1.25    7/9/04 4:25:20 PM  RLebeau
  Renamed ToBytes(raw) to RawToBytes() to fix an ambiquity error with
  ToBytes(TIdBytes)

  Rev 1.24    7/9/04 4:07:06 PM  RLebeau
  Compiler fix for TIdBaseStream.Write()

  Rev 1.23    09/07/2004 22:17:52  ANeillans
  Fixed IdGlobal.pas(761) Error: ';', ')' or '=' expected but ':=' found

  Rev 1.22    7/8/04 11:56:10 PM  RLebeau
  Added additional parameters to BytesToString()

  Bug fix for ReadStringFromStream()

  Updated TIdBaseStream.Write() to use ToBytes()

  Rev 1.21    7/8/04 4:22:36 PM  RLebeau
  Added ToBytes() overload for raw pointers under non-DotNet platfoms.

  Rev 1.20    2004.07.03 19:39:38  czhower
  UTF8

  Rev 1.19    6/15/2004 7:18:06 PM  JPMugaas
  IdInsert for stuff needing to call the Insert procedure.

  Rev 1.18    2004.06.13 8:06:46 PM  czhower
  .NET update

    Rev 1.17    6/11/2004 8:28:30 AM  DSiders
  Added "Do not Localize" comments.

  Rev 1.16    2004.06.08 7:11:14 PM  czhower
  Typo fix.

  Rev 1.15    2004.06.08 6:34:48 PM  czhower
  .NET bug with Ticks workaround.

  Rev 1.14    07/06/2004 21:30:32  CCostelloe
  Kylix 3 changes

  Rev 1.13    5/3/04 12:17:44 PM  RLebeau
  Updated ToBytes(string) and BytesToString() under DotNet to use
  System.Text.Encoding.ASCII instead of AnsiEncoding

  Rev 1.12    4/24/04 12:41:36 PM  RLebeau
  Conversion support to/from TIdBytes for Char values

  Rev 1.11    4/18/04 2:45:14 PM  RLebeau
  Conversion support to/from TIdBytes for Int64 values

  Rev 1.10    2004.04.08 4:50:06 PM  czhower
  Comments

  Rev 1.9    2004.04.08 1:45:42 AM  czhower
  tiny string optimization

  Rev 1.8    4/7/2004 3:20:50 PM  JPMugaas
  PosIdx was not working in DotNET.  In DotNET, it was returning a Pos value
  without adding the startvalue -1.  It was throwing off the FTP list parsers.

  Two uneeded IFDEF's were removed.

  Rev 1.7    2004.03.13 5:51:28 PM  czhower
  Fixed stack overflow in Sleep for .net

  Rev 1.6    3/6/2004 5:16:02 PM  JPMugaas
  Bug 67 fixes.  Do not write to const values.

  Rev 1.5    3/6/2004 4:54:12 PM  JPMugaas
  Write to const bug fix.

  Rev 1.4    2/17/2004 12:02:44 AM  JPMugaas
  A few routines that might be needed later for RFC 3490 support.

  Rev 1.3    2/16/2004 1:56:04 PM  JPMugaas
  Moved some routines here to lay the groundwork for RFC 3490 support.  Started
  work on RFC 3490 support.

  Rev 1.2    2/11/2004 5:12:30 AM  JPMugaas
  Moved IPv6 address definition here.

  I also made a function for converting a TIdBytes to an IPv6 address.

  Rev 1.1    2004.02.03 3:15:52 PM  czhower
  Updates to move to System.

  Rev 1.0    2004.02.03 2:28:30 PM  czhower
  Move

  Rev 1.91    2/1/2004 11:16:04 PM  BGooijen
  ToBytes

  Rev 1.90    2/1/2004 1:28:46 AM  JPMugaas
  Disabled IdPort functionality in DotNET.  It can't work there in it's current
  form and trying to get it to work will introduce more problems than it
  solves.  It was only used by the bindings editor and we did something
  different in DotNET so IdPorts wouldn't used there.

  Rev 1.89    2004.01.31 1:51:10 AM  czhower
  IndyCast for VB.

  Rev 1.88    30/1/2004 4:47:46 PM  SGrobety
  Added "WriteMemoryStreamToStream" to take care of Win32/dotnet difference in
  the TMemoryStream.Memory type and the Write buffer parameter

  Rev 1.87    1/30/2004 11:59:24 AM  BGooijen
  Added WriteTIdBytesToStream, because we can convert almost everything to
  TIdBytes, and TIdBytes couldn't be written to streams easily

  Rev 1.86    2004.01.27 11:44:36 PM  czhower
  .Net Updates

  Rev 1.85    2004.01.27 8:15:54 PM  czhower
  Fixed compile error + .net helper.

  Rev 1.84    27/1/2004 1:55:10 PM  SGrobety
  TIdStringStream introduced to fix a bug in DOTNET TStringStream
  implementation.

  Rev 1.83    2004.01.27 1:42:00 AM  czhower
  Added parameter check

  Rev 1.82    25/01/2004 21:55:40  CCostelloe
  Added portable IdFromBeginning/FromCurrent/FromEnd, to be used instead of
  soFromBeginning/soBeginning, etc.

  Rev 1.81    24/01/2004 20:18:46  CCostelloe
  Added IndyCompareStr (to be used in place of AnsiCompareStr for .NET
  compatibility)

  Rev 1.80    2004.01.23 9:56:30 PM  czhower
  CharIsInSet now checks length and returns false if no character.

  Rev 1.79    2004.01.23 9:49:40 PM  czhower
  CharInSet no longer accepts -1, was unneeded and redundant.

  Rev 1.78    1/22/2004 5:47:46 PM  SPerry
  fixed CharIsInSet

  Rev 1.77    2004.01.22 5:33:46 PM  czhower
  TIdCriticalSection

  Rev 1.76    2004.01.22 3:23:18 PM  czhower
  IsCharInSet

  Rev 1.75    2004.01.22 2:00:14 PM  czhower
  iif change

  Rev 1.74    14/01/2004 00:17:34  CCostelloe
  Added IndyLowerCase/IndyUpperCase to replace AnsiLowerCase/AnsiUpperCase for
  .NET code

  Rev 1.73    1/11/2004 9:50:54 PM  BGooijen
  Added ToBytes function for Socks

  Rev 1.72    2003.12.31 7:32:40 PM  czhower
  InMainThread now for .net too.

  Rev 1.71    2003.12.29 6:48:38 PM  czhower
  TextIsSame

  Rev 1.70    2003.12.28 1:11:04 PM  czhower
  Conditional typo fixed.

  Rev 1.69    2003.12.28 1:05:48 PM  czhower
  .Net changes.

  Rev 1.68    5/12/2003 9:11:00 AM  GGrieve
  Add WriteStringToStream

  Rev 1.67    5/12/2003 12:32:48 AM  GGrieve
  fix DotNet warnings

  Rev 1.66    22/11/2003 12:03:02 AM  GGrieve
  fix IdMultiPathFormData.pas implementation

  Rev 1.65    11/15/2003 1:15:36 PM  VVassiliev
  Move AppendByte from IdDNSCommon to IdCoreGlobal

  Rev 1.64    10/28/2003 8:43:48 PM  BGooijen
  compiles, and removed call to setstring

  Rev 1.63    2003.10.24 10:44:50 AM  czhower
  IdStream implementation, bug fixes.

  Rev 1.62    10/18/2003 4:53:18 PM  BGooijen
  Added ToHex

  Rev 1.61    2003.10.17 6:17:24 PM  czhower
  Some parts moved to stream

    Rev 1.60    10/15/2003 8:28:16 PM  DSiders
  Added localization comments.

  Rev 1.59    2003.10.14 9:27:12 PM  czhower
  Fixed compile erorr with missing )

  Rev 1.58    10/14/2003 3:31:04 PM  SPerry
  Modified ByteToHex() and IPv4ToHex

  Rev 1.57    10/13/2003 5:06:46 PM  BGooijen
  Removed local constant IdOctalDigits in favor of the unit constant. - attempt
  2

    Rev 1.56    10/13/2003 10:07:12 AM  DSiders
  Reverted prior change; local constant for IdOctalDigits is restored.

    Rev 1.55    10/12/2003 11:55:42 AM  DSiders
  Removed local constant IdOctalDigits in favor of the unit constant.

  Rev 1.54    2003.10.11 5:47:22 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.53    10/8/2003 10:14:34 PM  GGrieve
  add WriteStringToStream

  Rev 1.52    10/8/2003 9:55:30 PM  GGrieve
  Add IdDelete

  Rev 1.51    10/7/2003 11:33:30 PM  GGrieve
  Fix ReadStringFromStream

  Rev 1.50    10/7/2003 10:07:30 PM  GGrieve
  Get IdHTTP compiling for DotNet

  Rev 1.49    6/10/2003 5:48:48 PM  SGrobety
  DotNet updates

  Rev 1.48    10/5/2003 12:26:46 PM  BGooijen
  changed parameter names at some places

  Rev 1.47    10/4/2003 7:08:26 PM  BGooijen
  added some conversion routines type->TIdBytes->type, and fixed existing ones

  Rev 1.46    10/4/2003 3:53:40 PM  BGooijen
  added some ToBytes functions

  Rev 1.45    04/10/2003 13:38:28  HHariri
  Write(Integer) support

  Rev 1.44    10/3/2003 10:44:54 PM  BGooijen
  Added WriteBytesToStream

  Rev 1.43    2003.10.02 8:29:14 PM  czhower
  Changed names of byte conversion routines to be more readily understood and
  not to conflict with already in use ones.

  Rev 1.42    10/2/2003 5:15:16 PM  BGooijen
  Added Grahame's functions

  Rev 1.41    10/1/2003 8:02:20 PM  BGooijen
  Removed some ifdefs and improved code

  Rev 1.40    2003.10.01 9:10:58 PM  czhower
  .Net

  Rev 1.39    2003.10.01 2:46:36 PM  czhower
  .Net

  Rev 1.38    2003.10.01 2:30:36 PM  czhower
  .Net

  Rev 1.37    2003.10.01 12:30:02 PM  czhower
  .Net

  Rev 1.35    2003.10.01 1:12:32 AM  czhower
  .Net

  Rev 1.34    2003.09.30 7:37:14 PM  czhower
  Typo fix.

  Rev 1.33    30/9/2003 3:58:08 PM  SGrobety
  More .net updates

  Rev 1.31    2003.09.30 3:19:30 PM  czhower
  Updates for .net

  Rev 1.30    2003.09.30 1:22:54 PM  czhower
  Stack split for DotNet

  Rev 1.29    2003.09.30 12:09:36 PM  czhower
  DotNet changes.

  Rev 1.28    2003.09.30 10:36:02 AM  czhower
  Moved stack creation to IdStack
  Added DotNet stack.

  Rev 1.27    9/29/2003 03:03:28 PM  JPMugaas
  Changed CIL to DOTNET.

  Rev 1.26    9/28/2003 04:22:00 PM  JPMugaas
  IFDEF'ed out MemoryPos in NET because that will not work there.

  Rev 1.25    9/26/03 11:20:50 AM  RLebeau
  Updated defines used with SetThreadName() to allow it to work under BCB6.

  Rev 1.24    9/24/2003 11:42:42 PM  JPMugaas
  Minor changes to help compile under NET

  Rev 1.23    2003.09.20 10:25:42 AM  czhower
  Added comment and chaned for D6 compat.

  Rev 1.22    9/18/2003 07:43:12 PM  JPMugaas
  Moved GetThreadHandle to IdGlobals so the ThreadComponent can be in this
  package.

  Rev 1.21    9/8/2003 11:44:38 AM  JPMugaas
  Fix for problem that was introduced in an optimization.

  Rev 1.20    2003.08.19 1:54:34 PM  czhower
  Removed warning

  Rev 1.19    11/8/2003 6:25:44 PM  SGrobety
  IPv4ToDWord: Added overflow checking disabling ($Q+) and changed "* 256"  by
  "SHL 8".

  Rev 1.18    2003.07.08 2:41:42 PM  czhower
  This time I saved the file before checking in.

  Rev 1.16    7/1/2003 03:39:38 PM  JPMugaas
  Started numeric IP function API calls for more efficiency.

  Rev 1.15    2003.07.01 3:49:56 PM  czhower
  Added SetThreadName

    Rev 1.14    7/1/2003 12:03:56 AM  BGooijen
  Added functions to switch between IPv6 addresses in string and in
  TIdIPv6Address form

  Rev 1.13    6/30/2003 06:33:58 AM  JPMugaas
  Fix for range check error.

  Rev 1.12    6/27/2003 04:43:30 PM  JPMugaas
  Made IPv4ToDWord overload that returns a flag for an error message.
  Moved MakeCanonicalIPv4Address code into IPv4ToDWord because most of that
  simply reduces IPv4 addresses into a DWord.  That also should make the
  function more useful in reducing various alternative forms of IPv4 addresses
  down to DWords.

  Rev 1.11    6/27/2003 01:19:38 PM  JPMugaas
  Added MakeCanonicalIPv4Address for converting various IPv4 address forms
  (mentioned at http://www.pc-help.org/obscure.htm) into a standard dotted IP
  address.  Hopefully, we should soon support octal and hexidecimal addresses.

  Rev 1.9    6/27/2003 04:36:08 AM  JPMugaas
  Function for converting DWord to IP adcdress.

  Rev 1.8    6/26/2003 07:54:38 PM  JPMugaas
  Routines for converting standard dotted IPv4 addresses into dword,
  hexidecimal, and octal forms.

    Rev 1.7    5/11/2003 11:57:06 AM  BGooijen
  Added RaiseLastOSError

  Rev 1.6    4/28/2003 03:19:00 PM  JPMugaas
  Made a function for obtaining the services file FQN.  That's in case
  something else besides IdPorts needs it.

  Rev 1.5    2003.04.16 10:06:42 PM  czhower
  Moved DebugOutput to IdCoreGlobal

  Rev 1.4    12/29/2002 2:15:30 PM  JPMugaas
  GetCurrentThreadHandle function created as per Bas's instructions.  Moved
  THandle to IdCoreGlobal for this function.

  Rev 1.3    12-15-2002 17:02:58  BGooijen
  Added comments to TIdExtList

  Rev 1.2    12-15-2002 16:45:42  BGooijen
  Added TIdList

  Rev 1.1    29/11/2002 10:08:50 AM  SGrobety    Version: 1.1
  Changed GetTickCount to use high-performance timer if available under windows

  Rev 1.0    21/11/2002 12:36:18 PM  SGrobety    Version: Indy 10

  Rev 1.0    11/13/2002 08:41:24 AM  JPMugaas
}

unit IdGlobal;

interface

{$I IdCompilerDefines.inc}

uses
  SysUtils,
  {$IFDEF DOTNET}
  System.Collections.Specialized,
  System.net,
  System.net.Sockets,
  System.Diagnostics,
  System.Threading,
  System.IO,
  System.Text,
  {$ELSE}
    {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WINDOWS}
    {$IFDEF FPC}
  windows,
    {$ELSE}
  Windows,
    {$ENDIF}
  {$ENDIF}
  Classes,
  syncobjs,
  {$IFDEF UNIX}
    {$IFDEF KYLIXCOMPAT}
    Libc,
    {$ELSE}
      {$IFDEF FPC}
      DynLibs, // better add DynLibs only for fpc
      {$ENDIF}
      {$IFDEF USE_VCL_POSIX}
      Posix.SysTypes, Posix.Pthread, Posix.Unistd,
      {$ENDIF}
      {$IFDEF USE_BASEUNIX}
      BaseUnix, Unix, Sockets, UnixType, 
      {$ENDIF}
      {$IFDEF USE_ICONV_ENC}iconvenc, {$ENDIF}
    {$ENDIF}
    {$IFDEF DARWIN}
      {$IFNDEF FPC}
      //RLebeau: FPC does not provide mach_timebase_info() and mach_absolute_time() yet...
      Macapi.Mach,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  IdException;

{$IFNDEF DOTNET}
  {$IFNDEF HAS_PCardinal}
type
  PCardinal = ^Cardinal;
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_QWord}
  {$IFNDEF HAS_PQWord}
type
  PQWord = ^QWord;
  {$ENDIF}
{$ENDIF}

{$IFNDEF HAS_Int8}
type
  Int8 = {$IFDEF DOTNET}System.SByte{$ELSE}Shortint{$ENDIF};
  {$NODEFINE Int8}
{$ENDIF}
{$IFNDEF HAS_PInt8}
  {$IFNDEF DOTNET}
type
  PInt8 = PShortint;
  {$NODEFINE PInt8}
  {$ENDIF}
{$ENDIF}

{$IFNDEF HAS_UInt8}
type
  UInt8 = {$IFDEF DOTNET}System.Byte{$ELSE}Byte{$ENDIF};
  {$NODEFINE UInt8}
{$ENDIF}
{$IFNDEF HAS_PUInt8}
  {$IFNDEF DOTNET}
type
  PUInt8 = PByte;
  {$NODEFINE PUInt8}
  {$ENDIF}
{$ENDIF}

{$IFNDEF HAS_Int16}
type
  Int16 = Smallint;
  {$NODEFINE Int16}
{$ENDIF}
{$IFNDEF HAS_PInt16}
  {$IFNDEF DOTNET}
type
  PInt16 = PSmallint;
  {$NODEFINE PInt16}
  {$ENDIF}
{$ENDIF}

{$IFNDEF HAS_UInt16}
type
  UInt16 = Word;
  {$NODEFINE UInt16}
{$ENDIF}
{$IFNDEF HAS_PUInt16}
  {$IFNDEF DOTNET}
type
  PUInt16 = PWord;
  {$NODEFINE PUInt16}
  {$ENDIF}
{$ENDIF}

{$IFNDEF HAS_Int32}
type
  Int32 = Integer;
  {$NODEFINE Int32}
{$ENDIF}
{$IFNDEF HAS_PInt32}
  {$IFNDEF DOTNET}
type
  PInt32 = PInteger;
  {$NODEFINE PInt32}
  {$ENDIF}
{$ENDIF}

{$IFNDEF HAS_UInt32}
type
  UInt32 = Cardinal;
  {$NODEFINE UInt32}
{$ENDIF}
{$IFNDEF HAS_PUInt32}
  {$IFNDEF DOTNET}
type
  PUInt32 = PCardinal;
  {$NODEFINE PUInt32}
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_UInt64}
  {$DEFINE UInt64_IS_NATIVE}
  // In C++Builder 2006 and 2007, UInt64 is emitted as signed __int64 in HPP
  // files instead of as unsigned __int64.  This causes conflicts in overloaded
  // routines that have (U)Int64 parameters.  This was fixed in C++Builder 2009...
  {$IFNDEF TIdUInt64_HAS_QuadPart}
type
  TIdUInt64 = UInt64;
  {$ENDIF}
{$ELSE}
  {$IFDEF HAS_QWord}
    {$DEFINE UInt64_IS_NATIVE}
type
  UInt64 = QWord;
  {$NODEFINE UInt64}
  TIdUInt64 = QWord;
  {$ELSE}
type
  UInt64 = Int64;
  {$NODEFINE UInt64}
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_UInt64}
  {$IFNDEF HAS_PUInt64}
type
  PUInt64 = ^UInt64;
  {$ENDIF}
{$ELSE}
type
  PUInt64 = {$IFDEF HAS_QWord}PQWord{$ELSE}PInt64{$ENDIF};
{$ENDIF}

{$IFDEF TIdUInt64_HAS_QuadPart}
// For compilers that do not have a native UInt64 type, or for C++Builder
// 2006/2007 with its broken UInt64 HPP emit, let's define a record type
// that can hold UInt64 values, and then use it wherever UInt64 parameters
// are needed...
type
  TIdUInt64 = packed record
    case Integer of
    0: (
       {$IFDEF ENDIAN_BIG}
       HighPart: UInt32;
       LowPart: UInt32
       {$ELSE}
       LowPart: UInt32;
       HighPart: UInt32
       {$ENDIF}
       );
    1: (
       QuadPart: UInt64
       );
  end;
  {$NODEFINE TIdUInt64}

  (*$HPPEMIT 'namespace Idglobal'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT '    #pragma pack(push, 1)' *)
  (*$HPPEMIT '    struct TIdUInt64'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '        union {'*)
  (*$HPPEMIT '            struct {'*)
// TODO: move the endian check to the C++ side using #if...
  {$IFDEF ENDIAN_BIG}
  (*$HPPEMIT '                unsigned __int32 HighPart;'*)
  (*$HPPEMIT '                unsigned __int32 LowPart;'*)
  {$ELSE}
  (*$HPPEMIT '                unsigned __int32 LowPart;'*)
  (*$HPPEMIT '                unsigned __int32 HighPart;'*)
  {$ENDIF}
  (*$HPPEMIT '            };'*)
  (*$HPPEMIT '            unsigned __int64 QuadPart;'*)
  (*$HPPEMIT '        };'*)
  (*$HPPEMIT '        TIdUInt64(unsigned __int64 value) { QuadPart = value; }'*)
  (*$HPPEMIT '        operator unsigned __int64() const { return QuadPart; }'*)
  (*$HPPEMIT '        TIdUInt64& operator=(unsigned __int64 value) { QuadPart = value; return *this; }'*)
  (*$HPPEMIT '    };'*)
  (*$HPPEMIT '    #pragma pack(pop)' *)
  (*$HPPEMIT '}'*)
{$ENDIF}

const
  {This is the only unit with references to OS specific units and IFDEFs. NO OTHER units
  are permitted to do so except .pas files which are counterparts to dfm/xfm files, and only for
  support of that.}

  //We make the version things an Inc so that they can be managed independantly
  //by the package builder.
  {$I IdVers.inc}

  {$IFNDEF HAS_TIMEUNITS}
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;
  {$ENDIF}

  {$IFDEF DOTNET}
  // Timeout.Infinite is -1 which violates Cardinal which VCL uses for parameter
  // so we are just setting it to this as a hard coded constant until
  // the synchro classes and other are all ported directly to portable classes
  // (SyncObjs is platform specific)
  //Infinite = Timeout.Infinite;
  INFINITE = UInt32($FFFFFFFF);     { Infinite timeout }
  {$ENDIF}

  {$IFDEF KYLIX}
  NilHandle = 0;
  {$ENDIF}
  {$IFDEF DELPHI}
  NilHandle = 0;
  {$ENDIF}
  LF = #10;
  CR = #13;

  // RLebeau: EOL is NOT to be used as a platform-specific line break!  Most
  // text-based protocols that Indy implements are defined to use CRLF line
  // breaks. DO NOT change this!  If you need a platform-based line break,
  // use sLineBreak instead.
  EOL = CR + LF;
  //
  CHAR0 = #0;
  BACKSPACE = #8;

  TAB = #9;
  CHAR32 = #32;

  //Timeout values
  IdTimeoutDefault = -1;
  IdTimeoutInfinite = -2;
  //Fetch Defaults
  IdFetchDelimDefault = ' ';    {Do not Localize}
  IdFetchDeleteDefault = True;
  IdFetchCaseSensitiveDefault = True;

  IdWhiteSpace = [0..12, 14..32]; {do not localize}

  IdHexDigits: array [0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'); {do not localize}
  IdOctalDigits: array [0..7] of Char = ('0','1','2','3','4','5','6','7'); {do not localize}
  IdHexPrefix = '0x';  {Do not translate}

type
  //thread and PID stuff
  {$IFDEF DOTNET}
  TIdPID = UInt32;
  TIdThreadId = UInt32;
  TIdThreadHandle = System.Threading.Thread;
    {$IFDEF DOTNETDISTRO}
  TIdThreadPriority = System.Threading.ThreadPriority;
    {$ELSE}
  TIdThreadPriority = TThreadPriority;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF KYLIXCOMPAT}
  TIdPID = Int32;
  TIdThreadId = Int32;
      {$IFDEF FPC}
  TIdThreadHandle = TThreadID;
      {$ELSE}
  TIdThreadHandle = UInt32;
      {$ENDIF}
      {$IFDEF INT_THREAD_PRIORITY}
  TIdThreadPriority = -20..19;
      {$ELSE}
  TIdThreadPriority = TThreadPriority;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF USE_BASEUNIX}
  TIdPID = TPid;
  TIdThreadId = TThreadId;
  TIdThreadHandle = TIdThreadId;
  TIdThreadPriority = TThreadPriority;
    {$ENDIF}
    {$IFDEF USE_VCL_POSIX}
  TIdPID = pid_t;
  TIdThreadId = NativeUInt;
  TIdThreadHandle = NativeUInt;
      {$IFDEF INT_THREAD_PRIORITY}
  TIdThreadPriority = -20..19;
      {$ELSE}
  TIdThreadPriority = TThreadPriority;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WINDOWS}
  TIdPID = UInt32;
  TIdThreadId = UInt32;
  TIdThreadHandle = THandle;
    {$I IdSymbolPlatformOff.inc}
  TIdThreadPriority = TThreadPriority;
    {$I IdSymbolPlatformOn.inc}
  {$ENDIF}

  TIdTicks = UInt64;

  {$IFDEF INT_THREAD_PRIORITY}
const
  // approximate values, its finer grained on Linux
  tpIdle = 19;
  tpLowest = 12;
  tpLower = 6;
  tpNormal = 0;
  tpHigher = -7;
  tpHighest = -13;
  tpTimeCritical = -20;
  {$ENDIF}
  {CH tpIdLowest = tpLowest; }
  {CH tpIdBelowNormal = tpLower; }
  {CH tpIdNormal = tpNormal; }
  {CH tpIdAboveNormal = tpHigher; }
  {CH tpIdHighest = tpHighest; }
  //end thread stuff

const
  //leave this as zero.  It's significant in many socket calls that specify ports
  DEF_PORT_ANY = 0;

type
  {$IFDEF DOTNET}
  TIdUnicodeString = System.String;
  {$ELSE}
    {$IFDEF HAS_UnicodeString}
  TIdUnicodeString = UnicodeString;
    {$ELSE}
  TIdUnicodeString = WideString;
  // RP 9/12/2014: Synopse just released a unit that patches the System unit
  // in pre-Unicode versions of Delphi to redirect WideString memory management
  // to the RTL's memory manager (FastMM, etc) instead of the Win32 COM API!
  //
  // http://blog.synopse.info/post/2014/09/12/Faster-WideString-process-for-good-old-non-Unicode-Delphi-6-2007
  // https://github.com/synopse/mORMot/blob/master/SynFastWideString.pas
  //
  // We should consider providing an optional setting to enable that patch
  // so we can get a performance boost for Unicode-enabled code that uses
  // TIdUnicodeString...
    {$ENDIF}
  {$ENDIF}

  // the Delphi next-gen compiler eliminates AnsiString/AnsiChar/PAnsiChar,
  // but we still need to deal with Ansi data. Unfortunately, the compiler
  // won't let us use its secret _AnsiChr types either, so we have to use
  // Byte instead unless we can find a better solution...

  {$IFDEF HAS_AnsiChar}
  TIdAnsiChar = AnsiChar;
  {$ELSE}
  TIdAnsiChar = Byte;
  {$ENDIF}

  {$IFDEF HAS_PAnsiChar}
  PIdAnsiChar = PAnsiChar;
  {$ELSE}
    {$IFDEF HAS_MarshaledAString}
  PIdAnsiChar = MarshaledAString;
    {$ELSE}
  PIdAnsiChar = PByte;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF HAS_PPAnsiChar}
  PPIdAnsiChar = PPAnsiChar;
  {$ELSE}
  PPIdAnsiChar = ^PIdAnsiChar;
  {$ENDIF}

  {$IFDEF STRING_IS_UNICODE}
  TIdWideChar = Char;
  PIdWideChar = PChar;
  {$ELSE}
  TIdWideChar = WideChar;
  PIdWideChar = PWideChar;
  {$ENDIF}

  {$IFDEF WINDOWS}
  // .NET and Delphi 2009+ support UNICODE strings natively!
  //
  // FreePascal 2.4.0+ supports UnicodeString, but does not map its native
  // String type to UnicodeString except when {$MODE DelphiUnicode} or
  // {$MODESWITCH UnicodeStrings} is enabled.  However, UNICODE is not
  // defined in that mode yet until FreePascal's RTL has been updated to
  // support UnicodeString.  STRING_UNICODE_MISMATCH is defined in
  // IdCompilerDefines.inc when the compiler's native String/Char types do
  // not map to the same types that API functions are expecting based on
  // whether UNICODE is defined or not.  So we will create special Platform
  // typedefs here to help with API function calls when dealing with that
  // mismatch...
    {$IFDEF UNICODE}
  TIdPlatformString = TIdUnicodeString;
  TIdPlatformChar = TIdWideChar;
  PIdPlatformChar = PIdWideChar;
    {$ELSE}
  TIdPlatformString = AnsiString;
  TIdPlatformChar = TIdAnsiChar;
  PIdPlatformChar = PIdAnsiChar;
    {$ENDIF}
  {$ENDIF}

  TIdBytes = array of Byte;
  TIdWideChars = array of TIdWideChar;

  //NOTE:  The code below assumes a 32bit Linux architecture (such as target i386-linux)
  {$UNDEF CPU32_OR_KYLIX}
  {$IFNDEF DOTNET}
    {$IFDEF CPU32}
      {$DEFINE CPU32_OR_KYLIX}
    {$ENDIF}
    {$IFDEF KYLIX}
      {$DEFINE CPU32_OR_KYLIX}
    {$ENDIF}
  {$ENDIF}

  // native signed and unsigned integer sized pointer types

  {$IFDEF DOTNET}
  TIdNativeInt  = IntPtr;
  TIdNativeUInt = UIntPtr;
  {$ELSE}
    {$IFDEF HAS_NativeInt}
  TIdNativeInt = NativeInt;
    {$ELSE}
      {$IFDEF CPU32}
  TIdNativeInt = Int32;
      {$ENDIF}
      {$IFDEF CPU64}
  TIdNativeInt = Int64;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF HAS_NativeUInt}
  TIdNativeUInt = NativeUInt;
    {$ELSE}
      {$IFDEF CPU32}
  TIdNativeUInt = UInt32;
      {$ENDIF}
      {$IFDEF CPU64}
  TIdNativeUInt = UInt64;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF HAS_PtrInt}
  PtrInt = TIdNativeInt;
  {$ENDIF}
  {$IFNDEF HAS_PtrUInt}
  PtrUInt = TIdNativeUInt;
  {$ENDIF}

  {$IFDEF STREAM_SIZE_64}
  TIdStreamSize = Int64;
  {$ELSE}
  TIdStreamSize = Int32;
  {$ENDIF}

  {$IFNDEF HAS_SIZE_T}
  {$EXTERNALSYM size_t}
  size_t = PtrUInt;
  {$ENDIF}

  {$IFNDEF HAS_PSIZE_T}
  {$EXTERNALSYM Psize_t}
  Psize_t = ^size_t;
  {$ENDIF}

  {$IFDEF STRING_IS_IMMUTABLE}
  // In .NET and Delphi next-gen, strings are immutable (and zero-indexed), so we
  // need to use a StringBuilder whenever we need to modify individual characters
  // of a string...
  TIdStringBuilder = {$IFDEF DOTNET}System.Text.StringBuilder{$ELSE}TStringBuilder{$ENDIF};
  {$ENDIF}

  {
  Delphi/C++Builder 2009+ have a TEncoding class which mirrors System.Text.Encoding
  in .NET, but does not have a TDecoder class which mirrors System.Text.Decoder
  in .NET.  TEncoding's interface changes from version to version, in some ways
  that cause compatibility issues when trying to write portable code, so we will
  not rely on it.  IIdTextEncoding is our own wrapper so we have control over
  text encodings.

  This way, Indy can have a unified internal interface for String<->Byte conversions
  without using IFDEFs everywhere.

  Note: Having the wrapper class use WideString in earlier versions adds extra
  overhead to string operations, but this is the only way to ensure that strings
  are encoded properly.  Later on, perhaps we can optimize the operations when
  Ansi-compatible encodings are being used with AnsiString values.
  }

  {$IFNDEF HAS_IInterface}
  IInterface = IUnknown;
  {$ENDIF}

  IIdTextEncoding = interface(IInterface)
  ['{FA87FAE5-E3E3-4632-8FCA-2FB786848655}']
    function GetByteCount(const AChars: TIdWideChars): Integer; overload;
    function GetByteCount(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): Integer; overload;
    {$IFNDEF DOTNET}
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; overload;
    {$ENDIF}
    function GetByteCount(const AStr: TIdUnicodeString): Integer; overload;
    function GetByteCount(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): Integer; overload;
    function GetBytes(const AChars: TIdWideChars): TIdBytes; overload;
    function GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    {$IFNDEF DOTNET}
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; overload;
    {$ENDIF}
    function GetBytes(const AStr: TIdUnicodeString): TIdBytes; overload;
    function GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetCharCount(const ABytes: TIdBytes): Integer; overload;
    function GetCharCount(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): Integer; overload;
    {$IFNDEF DOTNET}
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; overload;
    {$ENDIF}
    function GetChars(const ABytes: TIdBytes): TIdWideChars; overload;
    function GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdWideChars; overload;
    function GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer; overload;
    {$IFNDEF DOTNET}
    function GetChars(const ABytes: PByte; AByteCount: Integer): TIdWideChars; overload;
    function GetChars(const ABytes: PByte; AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer; overload;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; overload;
    {$ENDIF}
    function GetIsSingleByte: Boolean;
    function GetMaxByteCount(ACharCount: Integer): Integer;
    function GetMaxCharCount(AByteCount: Integer): Integer;
    function GetPreamble: TIdBytes;
    function GetString(const ABytes: TIdBytes): TIdUnicodeString; overload;
    function GetString(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdUnicodeString; overload;
    {$IFNDEF DOTNET}
    function GetString(const ABytes: PByte; AByteCount: Integer): TIdUnicodeString; overload;
    {$ENDIF}
    property IsSingleByte: Boolean read GetIsSingleByte;
  end;

  IdTextEncodingType = (encIndyDefault, encOSDefault, enc8Bit, encASCII, encUTF16BE, encUTF16LE, encUTF7, encUTF8);

  function IndyTextEncoding(AType: IdTextEncodingType): IIdTextEncoding; overload;
  function IndyTextEncoding(ACodepage: UInt16): IIdTextEncoding; overload;
  function IndyTextEncoding(const ACharSet: String): IIdTextEncoding; overload;
  {$IFDEF DOTNET}
  function IndyTextEncoding(AEncoding: System.Text.Encoding): IIdTextEncoding; overload;
  {$ENDIF}
  {$IFDEF HAS_TEncoding}
  function IndyTextEncoding(AEncoding: TEncoding; AFreeEncoding: Boolean = False): IIdTextEncoding; overload;
  {$ENDIF}

  function IndyTextEncoding_Default: IIdTextEncoding;
  function IndyTextEncoding_OSDefault: IIdTextEncoding;
  function IndyTextEncoding_8Bit: IIdTextEncoding;
  function IndyTextEncoding_ASCII: IIdTextEncoding;
  function IndyTextEncoding_UTF16BE: IIdTextEncoding;
  function IndyTextEncoding_UTF16LE: IIdTextEncoding;
  function IndyTextEncoding_UTF7: IIdTextEncoding;
  function IndyTextEncoding_UTF8: IIdTextEncoding;

  // These are for backwards compatibility with past Indy 10 releases
  function enDefault: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_Default() or a nil IIdTextEncoding pointer'{$ENDIF};{$ENDIF}
  {$NODEFINE enDefault}
  function en7Bit: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_ASCII()'{$ENDIF};{$ENDIF}
  {$NODEFINE en7Bit}
  function en8Bit: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_8Bit()'{$ENDIF};{$ENDIF}
  {$NODEFINE en8Bit}
  function enUTF8: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_UTF8()'{$ENDIF};{$ENDIF}
  {$NODEFINE enUTF8}

  function Indy8BitEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_8Bit()'{$ENDIF};{$ENDIF}
  function IndyASCIIEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_ASCII()'{$ENDIF};{$ENDIF}
  function IndyUTF16BigEndianEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_UTF16BE()'{$ENDIF};{$ENDIF}
  function IndyUTF16LittleEndianEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_UTF16LE()'{$ENDIF};{$ENDIF}
  function IndyOSDefaultEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_OSDefault()'{$ENDIF};{$ENDIF}
  function IndyUTF7Encoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_UTF7()'{$ENDIF};{$ENDIF}
  function IndyUTF8Encoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IndyTextEncoding_UTF8()'{$ENDIF};{$ENDIF}

  (*$HPPEMIT '// These are helper macros to handle differences between C++Builder versions'*)
  (*$HPPEMIT '#define TIdTextEncoding_ASCII IndyTextEncoding_ASCII()'*)
  (*$HPPEMIT '#define TIdTextEncoding_BigEndianUnicode IndyTextEncoding_UTF16BE()'*)
  (*$HPPEMIT '#define TIdTextEncoding_Default IndyTextEncoding_OSDefault()'*)
  (*$HPPEMIT '#define TIdTextEncoding_Unicode IndyTextEncoding_UTF16LE()'*)
  (*$HPPEMIT '#define TIdTextEncoding_UTF7 IndyTextEncoding_UTF7()'*)
  (*$HPPEMIT '#define TIdTextEncoding_UTF8 IndyTextEncoding_UTF8()'*)
  (*$HPPEMIT ''*)

  (*$HPPEMIT '// These are for backwards compatibility with earlier Indy 10 releases'*)
  (*$HPPEMIT '#define enDefault ( ( IIdTextEncoding* )NULL )'*)
  (*$HPPEMIT '#define en8Bit IndyTextEncoding_8Bit()'*)
  (*$HPPEMIT '#define en7Bit IndyTextEncoding_ASCII()'*)
  (*$HPPEMIT '#define enUTF8 IndyTextEncoding_UTF8()'*)
  (*$HPPEMIT ''*)

var
  {RLebeau: using ASCII by default because most Internet protocols that Indy
  implements are based on ASCII specifically, not Ansi.  Non-ASCII data has
  to be explicitally allowed by RFCs, in which case the caller should not be
  using nil IIdTextEncoding objects to begin with...}
  GIdDefaultTextEncoding: IdTextEncodingType = encASCII;

  {$IFDEF USE_ICONV}
  // This indicates whether encOSDefault should map to an OS dependant Ansi
  // locale or to ASCII.  Defaulting to ASCII for now to maintain compatibility
  // with earlier Indy 10 releases...
  GIdIconvUseLocaleDependantAnsiEncoding: Boolean = False;

  // This indicates whether Iconv should ignore characters that cannot be
  // converted.  Defaulting to false for now to maintain compatibility with
  // earlier Indy 10 releases...
  GIdIconvIgnoreIllegalChars: Boolean = False;

  // This indicates whether Iconv should transliterate characters that cannot
  // be converted.  Defaulting to false for now to maintain compatibility with
  // earlier Indy 10 releases...
  GIdIconvUseTransliteration: Boolean = False;
  {$ENDIF}

procedure EnsureEncoding(var VEncoding : IIdTextEncoding; ADefEncoding: IdTextEncodingType = encIndyDefault);
procedure CheckByteEncoding(var VBytes: TIdBytes; ASrcEncoding, ADestEncoding: IIdTextEncoding);

type
  TIdAppendFileStream = class(TFileStream)
  public
    constructor Create(const AFile : String);
  end;

  TIdReadFileExclusiveStream = class(TFileStream)
  public
    constructor Create(const AFile : String);
  end;

  TIdReadFileNonExclusiveStream = class(TFileStream)
  public
    constructor Create(const AFile : String);
  end;

  TIdFileCreateStream = class(TFileStream)
  public
    constructor Create(const AFile : String);
  end;

  {$IFDEF DOTNET}
    {$IFNDEF DOTNET_2_OR_ABOVE}
  // dotNET implementation
  TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);

  TEvent = class(TObject)
  protected
    FEvent: WaitHandle;
  public
    constructor Create(EventAttributes: IntPtr; ManualReset,
      InitialState: Boolean; const Name: string = ''); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure SetEvent;
    procedure ResetEvent;
    function WaitFor(Timeout: UInt32): TWaitResult; virtual;
  end;

  TCriticalSection = class(TObject)
  public
    procedure Acquire; virtual;
    procedure Release; virtual;
    function TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
  end;
    {$ENDIF}
  {$ELSE}
    {$IFNDEF NO_REDECLARE}
 // TCriticalSection = SyncObjs.TCriticalSection;
    {$ENDIF}
  {$ENDIF}

  TIdLocalEvent = class(TEvent)
  public
    constructor Create(const AInitialState: Boolean = False;
     const AManualReset: Boolean = False); reintroduce;
    function WaitForEver: TWaitResult; overload;
  end;

  // This is here to reduce all the warnings about imports. We may also ifdef
  // it to provide a non warning implementatino on this unit too later.
  TIdCriticalSection = class(TCriticalSection)
  end;

  //Only needed for ToBytes(Short) and BytesToShort
  {$IFDEF DOTNET}
  Short = System.Int16 {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use Int16'{$ENDIF}{$ENDIF};
  {$ENDIF}
  {$IFDEF UNIX}
  Short = Int16 {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use Int16'{$ENDIF}{$ENDIF};
  {$ENDIF}

  {$IFNDEF DOTNET}
    {$IFNDEF NO_REDECLARE}
  PShort = ^Short;
    {$ENDIF}
  {$ENDIF}

  //This usually is a property editor exception
  EIdCorruptServicesFile = class(EIdException);
  EIdEndOfStream = class(EIdException);
  EIdInvalidIPv6Address = class(EIdException);
  EIdNoEncodingSpecified = class(EIdException);
  //This is called whenever there is a failure to retreive the time zone information
  EIdFailedToRetreiveTimeZoneInfo = class(EIdException);

  TIdPort = UInt16;

  //We don't have a native type that can hold an IPv6 address.
  {$NODEFINE TIdIPv6Address}
  TIdIPv6Address = array [0..7] of UInt16;

  // C++ does not allow an array to be returned by a function,
  // so wrapping the array in a struct as a workaround...
  //
  // This is one place where Word is being used instead of UInt16.
  // On OSX/iOS, UInt16 is defined in mactypes.h, not in System.hpp!
  // don't want to use a bunch of IFDEF's trying to figure out where
  // UInt16 is coming from...
  //
  (*$HPPEMIT 'namespace Idglobal'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT '    struct TIdIPv6Address'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '        ::System::Word data[8];'*)
  (*$HPPEMIT '        ::System::Word& operator[](int index) { return data[index]; }'*)
  (*$HPPEMIT '        const ::System::Word& operator[](int index) const { return data[index]; }'*)
  (*$HPPEMIT '        operator const ::System::Word*() const { return data; }'*)
  (*$HPPEMIT '        operator ::System::Word*() { return data; }'*)
  (*$HPPEMIT '    };'*)
  (*$HPPEMIT '}'*)

  {This way instead of a boolean for future expansion of other actions}
  TIdMaxLineAction = (maException, maSplit);
  TIdOSType = (otUnknown, otUnix, otWindows, otDotNet);
  //This is for IPv6 support when merged into the core
  TIdIPVersion = (Id_IPv4, Id_IPv6);

  {$IFNDEF NO_REDECLARE}
    {$IFDEF LINUX}
      {$IFNDEF VCL_6_OR_ABOVE}
  THandle = UInt32; //D6.System
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF DOTNET}
  THandle = Int32;
  {$ELSE}
    {$IFDEF WINDOWS}
//  THandle = Windows.THandle;
     {$ENDIF}
  {$ENDIF}

  TPosProc = function(const substr, str: String): Integer;
  {$IFNDEF DOTNET}
  TStrScanProc = function(Str: PChar; Chr: Char): PChar;
  {$ENDIF}
  TIdReuseSocket = (rsOSDependent, rsTrue, rsFalse);

  {$IFNDEF STREAM_SIZE_64}
  type
    TSeekOrigin = (soBeginning, soCurrent, soEnd);
  {$ENDIF}

  // TIdBaseStream is defined here to allow TIdMultiPartFormData to be defined
  // without any $IFDEFs in the unit IdMultiPartFormData - in accordance with Indy Coding rules
  TIdBaseStream = class(TStream)
  protected
    function IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint; virtual; abstract;
    function IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint; virtual; abstract;
    function IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; virtual; abstract;
    procedure IdSetSize(ASize: Int64); virtual; abstract;
    {$IFDEF DOTNET}
    procedure SetSize(ASize: Int64); override;
    {$ELSE}
      {$IFDEF STREAM_SIZE_64}
    procedure SetSize(const NewSize: Int64); override;
      {$ELSE}
    procedure SetSize(ASize: Integer); override;
      {$ENDIF}
    {$ENDIF}
  public
    {$IFDEF DOTNET}
    function Read(var VBuffer: array of Byte; AOffset, ACount: Longint): Longint; override;
    function Write(const ABuffer: array of Byte; AOffset, ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
      {$IFDEF STREAM_SIZE_64}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
      {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
      {$ENDIF}
    {$ENDIF}
  end;

  TIdCalculateSizeStream = class(TIdBaseStream)
  protected
    FPosition: Int64;
    FSize: Int64;
    function IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint; override;
    function IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint; override;
    function IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    procedure IdSetSize(ASize: Int64); override;
  end;

  TIdStreamReadEvent = procedure(var VBuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint) of object;
  TIdStreamWriteEvent = procedure(const ABuffer: TIdBytes; AOffset, ACount: Longint; var VResult: Longint) of object;
  TIdStreamSeekEvent = procedure(const AOffset: Int64; AOrigin: TSeekOrigin; var VPosition: Int64) of object;
  TIdStreamSetSizeEvent = procedure(const ANewSize: Int64) of object;

  TIdEventStream = class(TIdBaseStream)
  protected
    FOnRead: TIdStreamReadEvent;
    FOnWrite: TIdStreamWriteEvent;
    FOnSeek: TIdStreamSeekEvent;
    FOnSetSize: TIdStreamSetSizeEvent;
    function IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint; override;
    function IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint; override;
    function IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    procedure IdSetSize(ASize: Int64); override;
  public
    property OnRead: TIdStreamReadEvent read FOnRead write FOnRead;
    property OnWrite: TIdStreamWriteEvent read FOnWrite write FOnWrite;
    property OnSeek: TIdStreamSeekEvent read FOnSeek write FOnSeek;
    property OnSetSize: TIdStreamSetSizeEvent read FOnSetSize write FOnSetSize;
  end;

  {$IFNDEF DOTNET} // what is the .NET equivilent?
  TIdMemoryBufferStream = class(TCustomMemoryStream)
  public
    constructor Create(APtr: Pointer; ASize: TIdNativeInt);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;
  {$ENDIF}

const
  {$IFDEF UNIX}
  GOSType = otUnix;
  GPathDelim = '/'; {do not localize}
  INFINITE = UInt32($FFFFFFFF);     { Infinite timeout }
  {$ENDIF}

  {$IFDEF WINDOWS}
  GOSType = otWindows;
  GPathDelim = '\'; {do not localize}
  Infinite = Windows.INFINITE; { redeclare here for use elsewhere without using Windows.pas }  // cls modified 1/23/2002
  {$ENDIF}

  {$IFDEF DOTNET}
  GOSType = otDotNet;
  GPathDelim = '\'; {do not localize}
//  Infinite = ?; { redeclare here for use elsewhere without using Windows.pas }  // cls modified 1/23/2002
  {$ENDIF}

  // S.G. 4/9/2002: IP version general switch for defaults
  {$IFDEF IdIPv6}
  ID_DEFAULT_IP_VERSION = Id_IPv6;
  {$ELSE}
  ID_DEFAULT_IP_VERSION = Id_IPv4;
  {$ENDIF}

  {$IFNDEF HAS_sLineBreak}
    {$IFDEF WINDOWS}
  sLineBreak = CR + LF;
    {$ELSE}
  sLineBreak = LF;
    {$ENDIF}
  {$ENDIF}

//The power constants are for processing IP addresses
//They are powers of 255.
const
  POWER_1 = $000000FF;
  POWER_2 = $0000FFFF;
  POWER_3 = $00FFFFFF;
  POWER_4 = $FFFFFFFF;

// utility functions to calculate the usable length of a given buffer.
// If ALength is <0 then the actual Buffer length is returned,
// otherwise the minimum of the two lengths is returned instead.
function IndyLength(const ABuffer: String; const ALength: Integer = -1; const AIndex: Integer = 1): Integer; overload;
function IndyLength(const ABuffer: TIdBytes; const ALength: Integer = -1; const AIndex: Integer = 0): Integer; overload;
function IndyLength(const ABuffer: TStream; const ALength: TIdStreamSize = -1): TIdStreamSize; overload;

function IndyFormat(const AFormat: string; const Args: array of const): string;
function IndyIncludeTrailingPathDelimiter(const S: string): string;
function IndyExcludeTrailingPathDelimiter(const S: string): string;

procedure IndyRaiseLastError;

// This can only be called inside of an 'except' block! This is so that
// Exception.RaiseOuterException() (when available) can capture the current
// exception into the InnerException property of a new Exception that is
// being raised...
procedure IndyRaiseOuterException(AOuterException: Exception);

//You could possibly use the standard StrInt and StrIntDef but these
//also remove spaces from the string using the trim functions.
function IndyStrToInt(const S: string): Integer; overload;
function IndyStrToInt(const S: string; ADefault: Integer): Integer; overload;

function IndyFileAge(const AFileName: string): TDateTime;
function IndyDirectoryExists(const ADirectory: string): Boolean;

//You could possibly use the standard StrToInt and StrToInt64Def
//functions but these also remove spaces using the trim function
function IndyStrToInt64(const S: string; const ADefault: Int64): Int64; overload;
function IndyStrToInt64(const S: string): Int64;  overload;

//This converts the string to an Integer or Int64 depending on the bit size TStream uses
function IndyStrToStreamSize(const S: string; const ADefault: TIdStreamSize): TIdStreamSize; overload;
function IndyStrToStreamSize(const S: string): TIdStreamSize; overload;

function AddMSecToTime(const ADateTime: TDateTime; const AMSec: Integer): TDateTime;

// To and From Bytes conversion routines
function ToBytes(const AValue: string; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes; overload;
function ToBytes(const AValue: string; const ALength: Integer; const AIndex: Integer = 1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes; overload;
function ToBytes(const AValue: Char; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes; overload;
function ToBytes(const AValue: Int8): TIdBytes; overload;
function ToBytes(const AValue: UInt8): TIdBytes; overload;
function ToBytes(const AValue: Int16): TIdBytes; overload;
function ToBytes(const AValue: UInt16): TIdBytes; overload;
function ToBytes(const AValue: Int32): TIdBytes; overload;
function ToBytes(const AValue: UInt32): TIdBytes; overload;
function ToBytes(const AValue: Int64): TIdBytes; overload;
function ToBytes(const AValue: TIdUInt64): TIdBytes; overload;
function ToBytes(const AValue: TIdBytes; const ASize: Integer; const AIndex: Integer = 0): TIdBytes; overload;
{$IFNDEF DOTNET}
// RLebeau - not using the same "ToBytes" naming convention for RawToBytes()
// in order to prevent ambiquious errors with ToBytes(TIdBytes) above
function RawToBytes(const AValue; const ASize: Integer): TIdBytes;
{$ENDIF}

// The following functions are faster but except that Bytes[] must have enough
// space for at least SizeOf(AValue) bytes.
procedure ToBytesF(var Bytes: TIdBytes; const AValue: Char; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int8); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: UInt8); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int16); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: UInt16); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int32); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: UInt32); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int64); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: TIdUInt64); overload;
procedure ToBytesF(var Bytes: TIdBytes; const AValue: TIdBytes; const ASize: Integer; const AIndex: Integer = 0); overload;
{$IFNDEF DOTNET}
// RLebeau - not using the same "ToBytesF" naming convention for RawToBytesF()
// in order to prevent ambiquious errors with ToBytesF(TIdBytes) above
procedure RawToBytesF(var Bytes: TIdBytes; const AValue; const ASize: Integer);
{$ENDIF}

function ToHex(const AValue: TIdBytes; const ACount: Integer = -1; const AIndex: Integer = 0): string; overload;
function ToHex(const AValue: array of UInt32): string; overload; // for IdHash
function BytesToString(const AValue: TIdBytes; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;
function BytesToString(const AValue: TIdBytes; const AStartIndex: Integer;
  const ALength: Integer = -1; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;

// BytesToStringRaw() differs from BytesToString() in that it stores the
// byte octets as-is, whereas BytesToString() may decode character encodings
function BytesToStringRaw(const AValue: TIdBytes): string; overload;
function BytesToStringRaw(const AValue: TIdBytes; const AStartIndex: Integer;
  const ALength: Integer = -1): string; overload;

function BytesToChar(const AValue: TIdBytes; const AIndex: Integer = 0;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Char; overload;
function BytesToChar(const AValue: TIdBytes; var VChar: Char; const AIndex: Integer = 0;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Integer; overload;
function BytesToInt16(const AValue: TIdBytes; const AIndex: Integer = 0): Int16;
function BytesToUInt16(const AValue: TIdBytes; const AIndex : Integer = 0): UInt16;
function BytesToInt32(const AValue: TIdBytes; const AIndex: Integer = 0): Int32;
function BytesToUInt32(const AValue: TIdBytes; const AIndex : Integer = 0): UInt32;
function BytesToInt64(const AValue: TIdBytes; const AIndex: Integer = 0): Int64;
function BytesToUInt64(const AValue: TIdBytes; const AIndex: Integer = 0): TIdUInt64;

function BytesToShort(const AValue: TIdBytes; const AIndex: Integer = 0): Int16; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use BytesToInt16()'{$ENDIF};{$ENDIF}
function BytesToWord(const AValue: TIdBytes; const AIndex : Integer = 0): UInt16; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use BytesToUInt16()'{$ENDIF};{$ENDIF}
function BytesToLongInt(const AValue: TIdBytes; const AIndex: Integer = 0): Int32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use BytesToInt32()'{$ENDIF};{$ENDIF}
function BytesToLongWord(const AValue: TIdBytes; const AIndex : Integer = 0): UInt32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use BytesToUInt32()'{$ENDIF};{$ENDIF}

function BytesToIPv4Str(const AValue: TIdBytes; const AIndex: Integer = 0): String;
procedure BytesToIPv6(const AValue: TIdBytes; var VAddress: TIdIPv6Address; const AIndex: Integer = 0);
function BytesToTicks(const AValue: TIdBytes; const AIndex: Integer = 0): TIdTicks;
{$IFNDEF DOTNET}
procedure BytesToRaw(const AValue: TIdBytes; var VBuffer; const ASize: Integer);
{$ENDIF}

// TIdBytes utilities
procedure AppendBytes(var VBytes: TIdBytes; const AToAdd: TIdBytes; const AIndex: Integer = 0; const ALength: Integer = -1);
procedure AppendByte(var VBytes: TIdBytes; const AByte: Byte);
procedure AppendString(var VBytes: TIdBytes; const AStr: String; const ALength: Integer = -1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
procedure ExpandBytes(var VBytes: TIdBytes; const AIndex: Integer; const ACount: Integer; const AFillByte: Byte = 0);
procedure InsertBytes(var VBytes: TIdBytes; const ADestIndex: Integer; const ASource: TIdBytes; const ASourceIndex: Integer = 0);
procedure InsertByte(var VBytes: TIdBytes; const AByte: Byte; const AIndex: Integer);
procedure RemoveBytes(var VBytes: TIdBytes; const ACount: Integer; const AIndex: Integer = 0);

// Common Streaming routines
function ReadLnFromStream(AStream: TStream; var VLine: String; AMaxLineLength: Integer = -1;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Boolean; overload;
function ReadLnFromStream(AStream: TStream; AMaxLineLength: Integer = -1;
  AExceptionIfEOF: Boolean = False; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;
function ReadStringFromStream(AStream: TStream; ASize: Integer = -1; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;
procedure WriteStringToStream(AStream: TStream; const AStr: string; ADestEncoding: IIdTextEncoding
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;
procedure WriteStringToStream(AStream: TStream; const AStr: string; const ALength: Integer = -1;
  const AIndex: Integer = 1; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;
function ReadCharFromStream(AStream: TStream; var VChar: Char; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Integer;
function ReadTIdBytesFromStream(const AStream: TStream; var ABytes: TIdBytes;
  const Count: TIdStreamSize; const AIndex: Integer = 0): TIdStreamSize;
procedure WriteTIdBytesToStream(const AStream: TStream; const ABytes: TIdBytes;
  const ASize: Integer = -1; const AIndex: Integer = 0);

function ByteToHex(const AByte: Byte): string;
function ByteToOctal(const AByte: Byte): string;

function UInt32ToHex(const ALongWord : UInt32) : String;
function LongWordToHex(const ALongWord : UInt32) : String; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use UInt32ToHex()'{$ENDIF};{$ENDIF}

procedure CopyTIdBytes(const ASource: TIdBytes; const ASourceIndex: Integer;
  var VDest: TIdBytes; const ADestIndex: Integer; const ALength: Integer);

procedure CopyTIdByteArray(const ASource: array of Byte; const ASourceIndex: Integer;
  var VDest: TIdBytes; const ADestIndex: Integer; const ALength: Integer);

procedure CopyTIdChar(const ASource: Char; var VDest: TIdBytes; const ADestIndex: Integer;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
procedure CopyTIdInt16(const ASource: Int16; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdUInt16(const ASource: UInt16; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdInt32(const ASource: Int32; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdUInt32(const ASource: UInt32; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdInt64(const ASource: Int64; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdUInt64(const ASource: TIdUInt64; var VDest: TIdBytes; const ADestIndex: Integer);

procedure CopyTIdShort(const ASource: Int16; var VDest: TIdBytes; const ADestIndex: Integer); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use CopyTIdInt16()'{$ENDIF};{$ENDIF}
procedure CopyTIdWord(const ASource: UInt16; var VDest: TIdBytes; const ADestIndex: Integer); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use CopyTIdUInt16()'{$ENDIF};{$ENDIF}
procedure CopyTIdLongInt(const ASource: Int32; var VDest: TIdBytes; const ADestIndex: Integer); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use CopyTIdInt32()'{$ENDIF};{$ENDIF}
procedure CopyTIdLongWord(const ASource: UInt32; var VDest: TIdBytes; const ADestIndex: Integer); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use CopyTIdUInt32()'{$ENDIF};{$ENDIF}

procedure CopyTIdIPV6Address(const ASource: TIdIPv6Address; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdTicks(const ASource: TIdTicks; var VDest: TIdBytes; const ADestIndex: Integer);
procedure CopyTIdString(const ASource: String; var VDest: TIdBytes; const ADestIndex: Integer;
  const ALength: Integer = -1; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;
procedure CopyTIdString(const ASource: String; const ASourceIndex: Integer;
  var VDest: TIdBytes; const ADestIndex: Integer; const ALength: Integer = -1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;

// Need to change prob not to use this set
function CharPosInSet(const AString: string; const ACharPos: Integer; const ASet: String): Integer; {$IFDEF STRING_IS_IMMUTABLE}overload;{$ENDIF}
function CharIsInSet(const AString: string; const ACharPos: Integer; const ASet: String): Boolean; {$IFDEF STRING_IS_IMMUTABLE}overload;{$ENDIF}
function CharIsInEOL(const AString: string; const ACharPos: Integer): Boolean; {$IFDEF STRING_IS_IMMUTABLE}overload;{$ENDIF}
function CharEquals(const AString: string; const ACharPos: Integer; const AValue: Char): Boolean; {$IFDEF STRING_IS_IMMUTABLE}overload;{$ENDIF}

{$IFDEF STRING_IS_IMMUTABLE}
function CharPosInSet(const ASB: TIdStringBuilder; const ACharPos: Integer; const ASet: String): Integer; overload;
function CharIsInSet(const ASB: TIdStringBuilder; const ACharPos: Integer; const ASet: String): Boolean; overload;
function CharIsInEOL(const ASB: TIdStringBuilder; const ACharPos: Integer): Boolean; overload;
function CharEquals(const ASB: TIdStringBuilder; const ACharPos: Integer; const AValue: Char): Boolean; overload;
{$ENDIF}

function ByteIndex(const AByte: Byte; const ABytes: TIdBytes; const AStartIndex: Integer = 0): Integer;
function ByteIdxInSet(const ABytes: TIdBytes; const AIndex: Integer; const ASet: TIdBytes): Integer;
function ByteIsInSet(const ABytes: TIdBytes; const AIndex: Integer; const ASet: TIdBytes): Boolean;
function ByteIsInEOL(const ABytes: TIdBytes; const AIndex: Integer): Boolean;

function CompareDate(const D1, D2: TDateTime): Integer;
function CurrentProcessId: TIdPID;

// RLebeau: the input of these functions must be in GMT
function DateTimeGMTToHttpStr(const GMTValue: TDateTime) : String;
function DateTimeGMTToCookieStr(const GMTValue: TDateTime; const AUseNetscapeFmt: Boolean = True) : String;
function DateTimeGMTToImapStr(const GMTValue: TDateTime) : String;

// RLebeau: the input of these functions must be in local time
function DateTimeToInternetStr(const Value: TDateTime; const AUseGMTStr: Boolean = False) : String; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use LocalDateTimeToGMT()'{$ENDIF};{$ENDIF}
function DateTimeToGmtOffSetStr(ADateTime: TDateTime; const AUseGMTStr: Boolean = False): string; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use UTCOffsetToStr()'{$ENDIF};{$ENDIF}
function LocalDateTimeToHttpStr(const Value: TDateTime) : String;
function LocalDateTimeToCookieStr(const Value: TDateTime; const AUseNetscapeFmt: Boolean = True) : String;
function LocalDateTimeToImapStr(const Value: TDateTime) : String;
function LocalDateTimeToGMT(const Value: TDateTime; const AUseGMTStr: Boolean = False) : String;

procedure DebugOutput(const AText: string);
function Fetch(var AInput: string; const ADelim: string = IdFetchDelimDefault;
  const ADelete: Boolean = IdFetchDeleteDefault;
  const ACaseSensitive: Boolean = IdFetchCaseSensitiveDefault): string;
function FetchCaseInsensitive(var AInput: string; const ADelim: string = IdFetchDelimDefault;
  const ADelete: Boolean = IdFetchDeleteDefault): string;

// TODO: add an index parameter
procedure FillBytes(var VBytes : TIdBytes; const ACount : Integer; const AValue : Byte);

function CurrentThreadId: TIdThreadID;
function GetThreadHandle(AThread: TThread): TIdThreadHandle;

//GetTickDiff required because GetTickCount will wrap (IdICMP uses this)
function GetTickDiff(const AOldTickCount, ANewTickCount: UInt32): UInt32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use GetTickDiff64()'{$ENDIF};{$ENDIF}
function GetTickDiff64(const AOldTickCount, ANewTickCount: TIdTicks): TIdTicks;

// Most operations that use tick counters will never run anywhere near the
// 49.7 day limit that UInt32 imposes.  If an operation really were to
// run that long, use GetElapsedTicks64()...
function GetElapsedTicks(const AOldTickCount: TIdTicks): UInt32;
function GetElapsedTicks64(const AOldTickCount: TIdTicks): TIdTicks;

procedure IdDelete(var s: string; AOffset, ACount: Integer);
procedure IdInsert(const Source: string; var S: string; Index: Integer);

{$IFNDEF DOTNET}
type
  // TODO: use "array of Integer" instead?
  {$IFDEF HAS_GENERICS_TList}
  TIdPortList = TList<Integer>; // TODO: use TIdPort instead?
  {$ELSE}
  // TODO: flesh out to match TList<Integer> for non-Generics compilers
  TIdPortList = TList;
  {$ENDIF}

function IdPorts: TIdPortList;
{$ENDIF}

function iif(ATest: Boolean; const ATrue: Integer; const AFalse: Integer): Integer; overload;
function iif(ATest: Boolean; const ATrue: string; const AFalse: string = ''): string; overload; { do not localize }
function iif(ATest: Boolean; const ATrue: Boolean; const AFalse: Boolean): Boolean; overload;
function iif(const AEncoding, ADefEncoding: IIdTextEncoding; ADefEncodingType: IdTextEncodingType = encASCII): IIdTextEncoding; overload;

function InMainThread: Boolean;
function IPv6AddressToStr(const AValue: TIdIPv6Address): string;

//Note that there is NO need for Big Endian byte order functions because
//that's done through HostToNetwork byte order functions.
function HostToLittleEndian(const AValue : UInt16) : UInt16; overload;
function HostToLittleEndian(const AValue : UInt32): UInt32; overload;
function HostToLittleEndian(const AValue : Int32): Int32; overload;

function LittleEndianToHost(const AValue : UInt16) : UInt16; overload;
function LittleEndianToHost(const AValue : UInt32): UInt32; overload;
function LittleEndianToHost(const AValue : Int32): Int32; overload;

procedure WriteMemoryStreamToStream(Src: TMemoryStream; Dest: TStream; Count: TIdStreamSize);
{$IFNDEF DOTNET_EXCLUDE}
function IsCurrentThread(AThread: TThread): boolean;
{$ENDIF}
function IPv4ToUInt32(const AIPAddress: string): UInt32; overload;
function IPv4ToUInt32(const AIPAddress: string; var VErr: Boolean): UInt32; overload;
function IPv4ToDWord(const AIPAddress: string): UInt32; overload; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IPv4ToUInt32()'{$ENDIF};{$ENDIF}
function IPv4ToDWord(const AIPAddress: string; var VErr: Boolean): UInt32; overload; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IPv4ToUInt32()'{$ENDIF};{$ENDIF}
function IPv4ToHex(const AIPAddress: string; const ADotted: Boolean = False): string;
function IPv4ToOctal(const AIPAddress: string): string;
procedure IPv6ToIdIPv6Address(const AIPAddress: String; var VAddress: TIdIPv6Address); overload;
procedure IPv6ToIdIPv6Address(const AIPAddress: String; var VAddress: TIdIPv6Address; var VErr : Boolean); overload;
function IsAlpha(const AChar: Char): Boolean; overload;
function IsAlpha(const AString: String; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean; overload;
function IsAlphaNumeric(const AChar: Char): Boolean; overload;
function IsAlphaNumeric(const AString: String; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean; overload;
function IsASCII(const AByte: Byte): Boolean; overload;
function IsASCII(const ABytes: TIdBytes): Boolean; overload;
function IsASCIILDH(const AByte: Byte): Boolean; overload;
function IsASCIILDH(const ABytes: TIdBytes): Boolean; overload;
function IsHexidecimal(const AChar: Char): Boolean; overload;
function IsHexidecimal(const AString: string; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean; overload;
function IsNumeric(const AChar: Char): Boolean; overload;
function IsNumeric(const AString: string): Boolean; overload;
function IsNumeric(const AString: string; const ALength: Integer; const AIndex: Integer = 1): Boolean; overload;
function IsOctal(const AChar: Char): Boolean; overload;
function IsOctal(const AString: string; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean; overload;
{$IFNDEF DOTNET}
function InterlockedExchangeTHandle(var VTarget: THandle; const AValue: THandle): THandle;
function InterlockedCompareExchangePtr(var VTarget: Pointer; const AValue, Compare: Pointer): Pointer;
function InterlockedCompareExchangeObj(var VTarget: TObject; const AValue, Compare: TObject): TObject;
function InterlockedCompareExchangeIntf(var VTarget: IInterface; const AValue, Compare: IInterface): IInterface;
{$ENDIF}
function MakeCanonicalIPv4Address(const AAddr: string): string;
function MakeCanonicalIPv6Address(const AAddr: string): string;
function MakeUInt32IntoIPv4Address(const ADWord: UInt32): string;
function MakeDWordIntoIPv4Address(const ADWord: UInt32): string; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use MakeUInt32IntoIPv4Address()'{$ENDIF};{$ENDIF}
function IndyMin(const AValueOne, AValueTwo: Int64): Int64; overload;
function IndyMin(const AValueOne, AValueTwo: Int32): Int32; overload;
function IndyMin(const AValueOne, AValueTwo: UInt16): UInt16; overload;
function IndyMax(const AValueOne, AValueTwo: Int64): Int64; overload;
function IndyMax(const AValueOne, AValueTwo: Int32): Int32; overload;
function IndyMax(const AValueOne, AValueTwo: UInt16): UInt16; overload;
function IPv4MakeUInt32InRange(const AInt: Int64; const A256Power: Integer): UInt32;
function IPv4MakeLongWordInRange(const AInt: Int64; const A256Power: Integer): UInt32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use IPv4MakeUInt32InRange()'{$ENDIF};{$ENDIF}
{$IFNDEF DOTNET}
  {$IFDEF REGISTER_EXPECTED_MEMORY_LEAK}
function IndyRegisterExpectedMemoryLeak(AAddress: Pointer): Boolean;
  {$ENDIF}
{$ENDIF}
{$IFDEF UNIX}
function HackLoad(const ALibName : String; const ALibVersions : array of String) : HMODULE;
{$ENDIF}
{$IFNDEF DOTNET}
function MemoryPos(const ASubStr: string; MemBuff: PChar; MemorySize: Integer): Integer;
{$ENDIF}
function OffsetFromUTC: TDateTime;
function UTCOffsetToStr(const AOffset: TDateTime; const AUseGMTStr: Boolean = False): string;

function PosIdx(const ASubStr, AStr: string; AStartPos: UInt32 = 0): UInt32; //For "ignoreCase" use AnsiUpperCase
function PosInSmallIntArray(const ASearchInt: Int16; const AArray: array of Int16): Integer;
function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer;
{$IFNDEF DOTNET}
function ServicesFilePath: string;
{$ENDIF}
procedure IndySetThreadPriority(AThread: TThread; const APriority: TIdThreadPriority; const APolicy: Integer = -MaxInt);
procedure SetThreadName(const AName: string; {$IFDEF DOTNET}AThread: System.Threading.Thread = nil{$ELSE}AThreadID: UInt32 = $FFFFFFFF{$ENDIF});
procedure IndySleep(ATime: UInt32);

// TODO: create TIdStringPositionList for non-Nextgen compilers...
{$IFDEF USE_OBJECT_ARC}
type
  TIdStringPosition = record
    Value: String;
    Position: Integer;
    constructor Create(const AValue: String; const APosition: Integer);
  end;
  TIdStringPositionList = TList<TIdStringPosition>;
{$ENDIF}

//For non-Nextgen compilers: Integer(TStrings.Objects[i]) = column position in AData
//For Nextgen compilers: use SplitDelimitedString() if column positions are needed
procedure SplitColumnsNoTrim(const AData: string; AStrings: TStrings; const ADelim: string = ' '); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use SplitDelimitedString()'{$ENDIF};{$ENDIF} {Do not Localize}
procedure SplitColumns(const AData: string; AStrings: TStrings; const ADelim: string = ' '); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use SplitDelimitedString()'{$ENDIF};{$ENDIF}  {Do not Localize}

procedure SplitDelimitedString(const AData: string; AStrings: TStrings; ATrim: Boolean; const ADelim: string = ' '{$IFNDEF USE_OBJECT_ARC}; AIncludePositions: Boolean = False{$ENDIF}); {$IFDEF USE_OBJECT_ARC}overload;{$ENDIF}  {Do not Localize}
{$IFDEF USE_OBJECT_ARC}
procedure SplitDelimitedString(const AData: string; AStrings: TIdStringPositionList; ATrim: Boolean; const ADelim: string = ' '); overload;  {Do not Localize}
{$ENDIF}

function StartsWithACE(const ABytes: TIdBytes): Boolean;
function StringsReplace(const S: String; const OldPattern, NewPattern: array of string): string;
function ReplaceAll(const S, OldPattern, NewPattern: string): string;
function ReplaceOnlyFirst(const S, OldPattern, NewPattern: string): string;
function TextIsSame(const A1, A2: string): Boolean;
function TextStartsWith(const S, SubS: string): Boolean;
function TextEndsWith(const S, SubS: string): Boolean;
function IndyUpperCase(const A1: string): string;
function IndyLowerCase(const A1: string): string;
function IndyCompareStr(const A1: string; const A2: string): Integer;
function Ticks: UInt32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use Ticks64()'{$ENDIF};{$ENDIF}
function Ticks64: TIdTicks;
procedure ToDo(const AMsg: string);

function TwoByteToUInt16(AByte1, AByte2: Byte): UInt16;
function TwoByteToWord(AByte1, AByte2: Byte): UInt16; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use TwoByteToUInt16()'{$ENDIF};{$ENDIF}

function IndyAddPair(AStrings: TStrings; const AName, AValue: String): TStrings; overload;
function IndyAddPair(AStrings: TStrings; const AName, AValue: String; AObject: TObject): TStrings; overload;

function IndyIndexOf(AStrings: TStrings; const AStr: string; const ACaseSensitive: Boolean = False): Integer;{$IFDEF HAS_TStringList_CaseSensitive} overload;{$ENDIF}
{$IFDEF HAS_TStringList_CaseSensitive}
function IndyIndexOf(AStrings: TStringList; const AStr: string; const ACaseSensitive: Boolean = False): Integer; overload;
{$ENDIF}

function IndyIndexOfName(AStrings: TStrings; const AName: string; const ACaseSensitive: Boolean = False): Integer;{$IFDEF HAS_TStringList_CaseSensitive} overload;{$ENDIF}
{$IFDEF HAS_TStringList_CaseSensitive}
function IndyIndexOfName(AStrings: TStringList; const AName: string; const ACaseSensitive: Boolean = False): Integer; overload;
{$ENDIF}

function IndyValueFromIndex(AStrings: TStrings; const AIndex: Integer): String;

{$IFDEF WINDOWS}
function IndyWindowsMajorVersion: Integer;
function IndyWindowsMinorVersion: Integer;
function IndyWindowsBuildNumber: Integer;
function IndyWindowsPlatform: Integer;
function IndyCheckWindowsVersion(const AMajor: Integer; const AMinor: Integer = 0): Boolean;
{$ENDIF}

// For non-Nextgen compilers: IdDisposeAndNil is the same as FreeAndNil()
// For Nextgen compilers: IdDisposeAndNil calls TObject.DisposeOf() to ensure
// the object is freed immediately even if it has active references to it,
// for instance when freeing an Owned component
procedure IdDisposeAndNil(var Obj); {$IFDEF USE_INLINE}inline;{$ENDIF}

//RLebeau: FPC does not provide mach_timebase_info() and mach_absolute_time() yet...
{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF FPC}
type
  TTimebaseInfoData = record
    numer: UInt32;
    denom: UInt32;
  end;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

var
  {$IFDEF UNIX}

  // For linux the user needs to set this variable to be accurate where used (mail, etc)
  GOffsetFromUTC: TDateTime = 0{$IFDEF HAS_DEPRECATED}{$IFDEF USE_SEMICOLON_BEFORE_DEPRECATED};{$ENDIF} deprecated{$ENDIF};

    {$IFDEF DARWIN}
  GMachTimeBaseInfo: TTimebaseInfoData;
    {$ENDIF}
  {$ENDIF}

  IndyPos: TPosProc = nil;

{$IFDEF UNIX}
const
  {$IFDEF HAS_SharedSuffix}
  LIBEXT = '.' + SharedSuffix; {do not localize}
  {$ELSE}
    {$UNDEF LIBEXT_IS_DYLIB}
    {$IFDEF DARWIN}
      {$DEFINE LIBEXT_IS_DYLIB}
    {$ELSE}
      {$IFDEF IOS}
        {$DEFINE LIBEXT_IS_DYLIB}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF LIBEXT_IS_DYLIB}
  LIBEXT = '.dylib'; {do not localize}
    {$ELSE}
  LIBEXT = '.so'; {do not localize}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

implementation

{$IFDEF UNIX}
  {$IFDEF LINUX}
    {$DEFINE USE_clock_gettime}
    {$IFDEF FPC}
      {$linklib rt}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF FREEBSD}
    {$DEFINE USE_clock_gettime}
  {$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE USE_clock_gettime}
{$ENDIF}

uses
  {$IFDEF USE_VCL_POSIX}
  Posix.SysSelect,
  Posix.SysSocket,
  Posix.Time,
  Posix.SysTime,
  {$ENDIF}
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF DARWIN}
  Macapi.CoreServices,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF REGISTER_EXPECTED_MEMORY_LEAK}
    {$IFNDEF HAS_System_RegisterExpectedMemoryLeak}
      {$IFDEF USE_FASTMM4}FastMM4,{$ENDIF}
      {$IFDEF USE_MADEXCEPT}madExcept,{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_LIBC}Libc,{$ENDIF}
  {$IFDEF HAS_UNIT_DateUtils}DateUtils,{$ENDIF}
  //do not bring in our IdIconv unit if we are using the libc unit directly.
  {$IFDEF USE_ICONV_UNIT}IdIconv, {$ENDIF}
  IdResourceStrings,
  IdStream,
  {$IFDEF DOTNET}
  IdStreamNET
  {$ELSE}
  IdStreamVCL
  {$ENDIF}
  {$IFDEF HAS_PosEx}
    {$IFDEF HAS_UNIT_StrUtils}
  ,StrUtils
    {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF FPC}
  {$IFDEF WINCE}
  //FreePascal for WindowsCE may not define these.
const
  CP_UTF7 = 65000;
  CP_UTF8 = 65001;
  {$ENDIF}
{$ENDIF}

procedure EnsureEncoding(var VEncoding : IIdTextEncoding; ADefEncoding: IdTextEncodingType = encIndyDefault);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if VEncoding = nil then begin
    VEncoding := IndyTextEncoding(ADefEncoding);
  end;
end;

procedure CheckByteEncoding(var VBytes: TIdBytes; ASrcEncoding, ADestEncoding: IIdTextEncoding);
begin
  if ASrcEncoding <> ADestEncoding then begin
    VBytes := ADestEncoding.GetBytes(ASrcEncoding.GetChars(VBytes));
  end;
end;

{$IFNDEF WINDOWS}
//FreePascal may not define this for non-Windows systems.
//#define MAKEWORD(a, b)      ((WORD)(((BYTE)(a)) | ((WORD)((BYTE)(b))) << 8))
function MakeWord(const a, b : Byte) : Word;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := Word(a) or (Word(b) shl 8);
end;
{$ENDIF}

{$IFNDEF DOTNET}
var
  // TODO: use "array of Integer" instead?
  GIdPorts: TIdPortList = nil;

  GIdOSDefaultEncoding: IIdTextEncoding = nil;
  GId8BitEncoding: IIdTextEncoding = nil;
  GIdASCIIEncoding: IIdTextEncoding = nil;
  GIdUTF16BigEndianEncoding: IIdTextEncoding = nil;
  GIdUTF16LittleEndianEncoding: IIdTextEncoding = nil;
  GIdUTF7Encoding: IIdTextEncoding = nil;
  GIdUTF8Encoding: IIdTextEncoding = nil;
{$ENDIF}

{ IIdTextEncoding implementations }

{$IFDEF DOTNET}
type
  TIdDotNetEncoding = class(TInterfacedObject, IIdTextEncoding)
  protected
    FEncoding: System.Text.Encoding;
  public
    constructor Create(AEncoding: System.Text.Encoding); overload;
    constructor Create(const ACharset: String); overload;
    constructor Create(const ACodepage: UInt16); overload;
    function GetByteCount(const AChars: TIdWideChars): Integer; overload;
    function GetByteCount(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): Integer; overload;
    function GetByteCount(const AStr: TIdUnicodeString): Integer; overload;
    function GetByteCount(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): Integer; overload;
    function GetBytes(const AChars: TIdWideChars): TIdBytes; overload;
    function GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetBytes(const AStr: TIdUnicodeString): TIdBytes; overload;
    function GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetCharCount(const ABytes: TIdBytes): Integer; overload;
    function GetCharCount(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): Integer; overload;
    function GetChars(const ABytes: TIdBytes): TIdWideChars; overload;
    function GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdWideChars; overload;
    function GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer; overload;
    function GetIsSingleByte: Boolean;
    function GetMaxByteCount(ACharCount: Integer): Integer;
    function GetMaxCharCount(AByteCount: Integer): Integer;
    function GetPreamble: TIdBytes;
    function GetString(const ABytes: TIdBytes): TIdUnicodeString; overload;
    function GetString(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdUnicodeString; overload;
  end;

constructor TIdDotNetEncoding.Create(AEncoding: System.Text.Encoding);
begin
  inherited Create;
  FEncoding := AEncoding;
end;

constructor TIdDotNetEncoding.Create(const ACharset: String);
begin
  inherited Create;
  // RLebeau 5/2/2017: have seen some malformed emails that use 'utf8'
  // instead of 'utf-8', so let's check for that...

  // RLebeau 9/27/2017: updating to handle a few more UTFs without hyphens...

  case PosInStrArray(ACharset, ['UTF7', 'UTF8', 'UTF16', 'UTF16LE', 'UTF16BE', 'UTF32', 'UTF32LE', 'UTF32BE'], False) of {Do not Localize}
    0:   FEncoding := System.Text.Encoding.UTF7;
    1:   FEncoding := System.Text.Encoding.UTF8;
    2,3: FEncoding := System.Text.Encoding.Unicode;
    4:   FEncoding := System.Text.Encoding.BigEndianUnicode;
    5,6: FEncoding := System.Text.Encoding.UTF32;
    7:   FEncoding := System.Text.Encoding.GetEncoding(12001);
  else
    FEncoding := System.Text.Encoding.GetEncoding(ACharset);
  end;
end;

constructor TIdDotNetEncoding.Create(const ACodepage: UInt16);
begin
  inherited Create;
  FEncoding := System.Text.Encoding.GetEncoding(ACodepage);
end;

function TIdDotNetEncoding.GetByteCount(const AChars: TIdWideChars): Integer;
begin
  Result := FEncoding.GetByteCount(AChars);
end;

function TIdDotNetEncoding.GetByteCount(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): Integer;
begin
  Result := FEncoding.GetByteCount(AChars, ACharIndex, ACharCount);
end;

function TIdDotNetEncoding.GetByteCount(const AStr: TIdUnicodeString): Integer;
begin
  Result := FEncoding.GetByteCount(AStr);
end;

function TIdDotNetEncoding.GetByteCount(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): Integer;
begin
  Result := FEncoding.GetByteCount(AStr.Substring(ACharIndex-1, ACharCount));
end;

function TIdDotNetEncoding.GetBytes(const AChars: TIdWideChars): TIdBytes;
begin
  Result := FEncoding.GetBytes(AChars);
end;

function TIdDotNetEncoding.GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): TIdBytes;
begin
  Result := FEncoding.GetBytes(AChars, ACharIndex, ACharCount);
end;

function TIdDotNetEncoding.GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer;
begin
  Result := FEncoding.GetBytes(AChars, ACharIndex, ACharCount, VBytes, AByteIndex);
end;

function TIdDotNetEncoding.GetBytes(const AStr: TIdUnicodeString): TIdBytes;
begin
  Result := FEncoding.GetBytes(AStr);
end;

function TIdDotNetEncoding.GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): TIdBytes;
begin
  Result := FEncoding.GetByteCount(AStr.Substring(ACharIndex-1, ACharCount));
end;

function TIdDotNetEncoding.GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer;
begin
  Result := FEncoding.GetBytes(AStr, ACharIndex-1, ACharCount, VBytes, AByteIndex);
end;

function TIdDotNetEncoding.GetCharCount(const ABytes: TIdBytes): Integer;
begin
  Result := FEncoding.GetCharCount(ABytes);
end;

function TIdDotNetEncoding.GetCharCount(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): Integer;
begin
  Result := FEncoding.GetCharCount(ABytes, AByteIndex, AByteCount);
end;

function TIdDotNetEncoding.GetChars(const ABytes: TIdBytes): TIdWideChars;
begin
  Result := FEncoding.GetChars(ABytes);
end;

function TIdDotNetEncoding.GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdWideChars;
begin
  Result := FEncoding.GetChars(ABytes, AByteIndex, AByteCount);
end;

function TIdDotNetEncoding.GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer;
begin
  Result := FEncoding.GetChars(ABytes, AByteIndex, AByteCount, VChars, ACharIndex);
end;

function TIdDotNetEncoding.GetIsSingleByte: Boolean;
begin
  Result := FEncoding.IsSingleByte;
end;

function TIdDotNetEncoding.GetMaxByteCount(ACharCount: Integer): Integer;
begin
  Result := FEncoding.GetMaxByteCount(ACharCount);
end;

function TIdDotNetEncoding.GetMaxCharCount(AByteCount: Integer): Integer;
begin
  Result := FEncoding.GetMaxCharCount(AByteCount);
end;

function TIdDotNetEncoding.GetPreamble: TIdBytes;
begin
  Result := fEncoding.GetPreamble;
end;

function TIdDotNetEncoding.GetString(const ABytes: TIdBytes): TIdUnicodeString;
begin
  Result := FEncoding.GetString(ABytes);
end;

function TIdDotNetEncoding.GetString(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdUnicodeString;
begin
  Result := FEncoding.GetString(ABytes, AByteIndex, AByteCount);
end;

{$ELSE}

type
  TIdTextEncodingBase = class(TInterfacedObject, IIdTextEncoding)
  protected
    FIsSingleByte: Boolean;
    FMaxCharSize: Integer;
  public
    function GetByteCount(const AChars: TIdWideChars): Integer; overload;
    function GetByteCount(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): Integer; overload;
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; overload; virtual; abstract;
    function GetByteCount(const AStr: TIdUnicodeString): Integer; overload;
    function GetByteCount(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): Integer; overload;
    function GetBytes(const AChars: TIdWideChars): TIdBytes; overload;
    function GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; overload; virtual; abstract;
    function GetBytes(const AStr: TIdUnicodeString): TIdBytes; overload;
    function GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): TIdBytes; overload;
    function GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer; overload;
    function GetCharCount(const ABytes: TIdBytes): Integer; overload;
    function GetCharCount(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): Integer; overload;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; overload; virtual; abstract;
    function GetChars(const ABytes: TIdBytes): TIdWideChars; overload;
    function GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdWideChars; overload;
    function GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer; overload;
    function GetChars(const ABytes: PByte; AByteCount: Integer): TIdWideChars; overload;
    function GetChars(const ABytes: PByte; AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer; overload;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; overload; virtual; abstract;
    function GetIsSingleByte: Boolean;
    function GetMaxByteCount(ACharCount: Integer): Integer; virtual; abstract;
    function GetMaxCharCount(AByteCount: Integer): Integer; virtual; abstract;
    function GetPreamble: TIdBytes; virtual;
    function GetString(const ABytes: TIdBytes): TIdUnicodeString; overload;
    function GetString(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdUnicodeString; overload;
    function GetString(const ABytes: PByte; AByteCount: Integer): TIdUnicodeString; overload;
  end;

  {$UNDEF SUPPORTS_CODEPAGE_ENCODING}
  {$IFNDEF USE_ICONV}
    {$IFDEF WINDOWS}
      {$DEFINE SUPPORTS_CODEPAGE_ENCODING}
    {$ENDIF}
    {$IFDEF HAS_LocaleCharsFromUnicode}
      {$DEFINE SUPPORTS_CODEPAGE_ENCODING}
    {$ENDIF}
  {$ENDIF}

  TIdMBCSEncoding = class(TIdTextEncodingBase)
  private
    {$IFDEF USE_ICONV}
    FCharSet: String;
    {$ELSE}
      {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
    FCodePage: UInt32;
    FMBToWCharFlags: UInt32;
    FWCharToMBFlags: UInt32;
      {$ENDIF}
    {$ENDIF}
  public
    constructor Create; overload; virtual;
    {$IFDEF USE_ICONV}
    constructor Create(const CharSet: String); overload; virtual;
    {$ELSE}
      {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
    constructor Create(CodePage: Integer); overload; virtual;
    constructor Create(CodePage, MBToWCharFlags, WCharToMBFlags: Integer); overload; virtual;
      {$ENDIF}
    {$ENDIF}
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TIdBytes; override;
  end;

  TIdUTF7Encoding = class(TIdMBCSEncoding)
  public
    constructor Create; override;
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
  end;

  TIdUTF8Encoding = class(TIdUTF7Encoding)
  public
    constructor Create; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TIdBytes; override;
  end;

  TIdUTF16LittleEndianEncoding = class(TIdTextEncodingBase)
  public
    constructor Create; virtual;
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TIdBytes; override;
  end;

  TIdUTF16BigEndianEncoding = class(TIdUTF16LittleEndianEncoding)
  public
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; overload; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; overload; override;
    function GetPreamble: TIdBytes; override;
  end;

  TIdASCIIEncoding = class(TIdTextEncodingBase)
  public
    constructor Create; virtual;
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; override;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; override;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; override;
    function GetMaxByteCount(ACharCount: Integer): Integer; override;
    function GetMaxCharCount(AByteCount: Integer): Integer; override;
  end;

  TId8BitEncoding = class(TIdTextEncodingBase)
  public
    constructor Create; virtual;
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; override;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; override;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; override;
    function GetMaxByteCount(ACharCount: Integer): Integer; override;
    function GetMaxCharCount(AByteCount: Integer): Integer; override;
  end;

  {$IFDEF HAS_TEncoding}
  TIdVCLEncoding = class(TIdTextEncodingBase)
  protected
    FEncoding: TEncoding;
    FFreeEncoding: Boolean;
  public
    constructor Create(AEncoding: TEncoding; AFreeEncoding: Boolean); overload;
    {$IFDEF HAS_TEncoding_GetEncoding_ByEncodingName}
    constructor Create(const ACharset: String); overload;
    {$ENDIF}
    constructor Create(const ACodepage: UInt16); overload;
    destructor Destroy; override;
    function GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer; override;
    function GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte; AByteCount: Integer): Integer; override;
    function GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer; override;
    function GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PIdWideChar; ACharCount: Integer): Integer; override;
    function GetMaxByteCount(ACharCount: Integer): Integer; override;
    function GetMaxCharCount(AByteCount: Integer): Integer; override;
  end;
  {$ENDIF}

{ TIdTextEncodingBase }

function ValidateChars(const AChars: TIdWideChars; ACharIndex, ACharCount: Integer): PIdWideChar;
var
  Len: Integer;
begin
  Len := Length(AChars);
  if (ACharIndex < 0) or (ACharIndex >= Len) then begin
    raise Exception.CreateResFmt(PResStringRec(@RSCharIndexOutOfBounds), [ACharIndex]);
  end;
  if ACharCount < 0 then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidCharCount), [ACharCount]);
  end;
  if (Len - ACharIndex) < ACharCount then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidCharCount), [ACharCount]);
  end;
  if ACharCount > 0 then begin
    Result := @AChars[ACharIndex];
  end else begin
    Result := nil;
  end;
end;

function ValidateBytes(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): PByte; overload;
var
  Len: Integer;
begin
  Len := Length(ABytes);
  if (AByteIndex < 0) or (AByteIndex >= Len) then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidDestinationIndex), [AByteIndex]);
  end;
  if (Len - AByteIndex) < AByteCount then begin
    raise Exception.CreateRes(PResStringRec(@RSInvalidDestinationArray));
  end;
  if AByteCount > 0 then begin
    Result := @ABytes[AByteIndex];
  end else begin
    Result := nil;
  end;
end;

function ValidateBytes(const ABytes: TIdBytes; AByteIndex, AByteCount, ANeeded: Integer): PByte; overload;
var
  Len: Integer;
begin
  Len := Length(ABytes);
  if (AByteIndex < 0) or (AByteIndex >= Len) then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidDestinationIndex), [AByteIndex]);
  end;
  if (Len - AByteIndex) < ANeeded then begin
    raise Exception.CreateRes(PResStringRec(@RSInvalidDestinationArray));
  end;
  if AByteCount > 0 then begin
    Result := @ABytes[AByteIndex];
  end else begin
    Result := nil;
  end;
end;

function ValidateStr(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): PIdWideChar;
begin
  if ACharIndex < 1 then begin
    raise Exception.CreateResFmt(PResStringRec(@RSCharIndexOutOfBounds), [ACharIndex]);
  end;
  if ACharCount < 0 then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidCharCount), [ACharCount]);
  end;
  if (Length(AStr) - ACharIndex + 1) < ACharCount then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidCharCount), [ACharCount]);
  end;
  if ACharCount > 0 then begin
    Result := @AStr[ACharIndex];
  end else begin
    Result := nil;
  end;
end;

function TIdTextEncodingBase.GetByteCount(const AChars: TIdWideChars): Integer;
begin
  if AChars <> nil then begin
    Result := GetByteCount(PIdWideChar(AChars), Length(AChars));
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetByteCount(const AChars: TIdWideChars;
  ACharIndex, ACharCount: Integer): Integer;
var
  LChars: PIdWideChar;
begin
  LChars := ValidateChars(AChars, ACharIndex, ACharCount);
  if LChars <> nil then begin
    Result := GetByteCount(LChars, ACharCount);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetByteCount(const AStr: TIdUnicodeString): Integer;
begin
  if AStr <> '' then begin
    Result := GetByteCount(PIdWideChar(AStr), Length(AStr));
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetByteCount(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): Integer;
var
  LChars: PIdWideChar;
begin
  LChars := ValidateStr(AStr, ACharIndex, ACharCount);
  if LChars <> nil then begin
    Result := GetByteCount(LChars, ACharCount);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetBytes(const AChars: TIdWideChars): TIdBytes;
begin
  if AChars <> nil then begin
    Result := GetBytes(PIdWideChar(AChars), Length(AChars));
  end else begin
    Result := nil;
  end;
end;

function TIdTextEncodingBase.GetBytes(const AChars: TIdWideChars;
  ACharIndex, ACharCount: Integer): TIdBytes;
var
  Len: Integer;
begin
  Result := nil;
  Len := GetByteCount(AChars, ACharIndex, ACharCount);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetBytes(@AChars[ACharIndex], ACharCount, PByte(Result), Len);
  end;
end;

function TIdTextEncodingBase.GetBytes(const AChars: TIdWideChars;
  ACharIndex, ACharCount: Integer; var VBytes: TIdBytes; AByteIndex: Integer): Integer;
begin
  Result := GetBytes(
    ValidateChars(AChars, ACharIndex, ACharCount),
    ACharCount, VBytes, AByteIndex);
end;

function TIdTextEncodingBase.GetBytes(const AChars: PIdWideChar; ACharCount: Integer): TIdBytes;
var
  Len: Integer;
begin
  Result := nil;
  Len := GetByteCount(AChars, ACharCount);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetBytes(AChars, ACharCount, PByte(Result), Len);
  end;
end;

function TIdTextEncodingBase.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  var VBytes: TIdBytes; AByteIndex: Integer): Integer;
var
  Len, LByteCount: Integer;
  LBytes: PByte;
begin
  if (AChars = nil) and (ACharCount <> 0) then begin
    raise Exception.CreateRes(PResStringRec(@RSInvalidSourceArray));
  end;
  if (VBytes = nil) and (ACharCount <> 0) then begin
    raise Exception.CreateRes(PResStringRec(@RSInvalidDestinationArray));
  end;
  if ACharCount < 0 then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidCharCount), [ACharCount]);
  end;
  Len := Length(VBytes);
  LByteCount := GetByteCount(AChars, ACharCount);
  LBytes := ValidateBytes(VBytes, AByteIndex, Len, LByteCount);
  Dec(Len, AByteIndex);
  if (ACharCount > 0) and (Len > 0) then begin
    Result := GetBytes(AChars, ACharCount, LBytes, LByteCount);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetBytes(const AStr: TIdUnicodeString): TIdBytes;
var
  Len: Integer;
begin
  Result := nil;
  Len := GetByteCount(AStr);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetBytes(PIdWideChar(AStr), Length(AStr), PByte(Result), Len);
  end;
end;

function TIdTextEncodingBase.GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer): TIdBytes;
var
  Len: Integer;
  LChars: PIdWideChar;
begin
  Result := nil;
  LChars := ValidateStr(AStr, ACharIndex, ACharCount);
  if LChars <> nil then begin
    Len := GetByteCount(LChars, ACharCount);
    if Len > 0 then begin
      SetLength(Result, Len);
      GetBytes(LChars, ACharCount, PByte(Result), Len);
    end;
  end;
end;

function TIdTextEncodingBase.GetBytes(const AStr: TIdUnicodeString; ACharIndex, ACharCount: Integer;
  var VBytes: TIdBytes; AByteIndex: Integer): Integer;
var
  LChars: PIdWideChar;
begin
  LChars := ValidateStr(AStr, ACharIndex, ACharCount);
  if LChars <> nil then begin
    Result := GetBytes(LChars, ACharCount, VBytes, AByteIndex);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetCharCount(const ABytes: TIdBytes): Integer;
begin
  if ABytes <> nil then begin
    Result := GetCharCount(PByte(ABytes), Length(ABytes));
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetCharCount(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): Integer;
var
  LBytes: PByte;
begin
  LBytes := ValidateBytes(ABytes, AByteIndex, AByteCount);
  if LBytes <> nil then begin
    Result := GetCharCount(LBytes, AByteCount);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetChars(const ABytes: TIdBytes): TIdWideChars;
begin
  if ABytes <> nil then begin
    Result := GetChars(PByte(ABytes), Length(ABytes));
  end else begin
    Result := nil;
  end;
end;

function TIdTextEncodingBase.GetChars(const ABytes: TIdBytes; AByteIndex, AByteCount: Integer): TIdWideChars;
var
  Len: Integer;
begin
  Result := nil;
  Len := GetCharCount(ABytes, AByteIndex, AByteCount);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetChars(@ABytes[AByteIndex], AByteCount, PIdWideChar(Result), Len);
  end;
end;

function TIdTextEncodingBase.GetChars(const ABytes: TIdBytes;
  AByteIndex, AByteCount: Integer; var VChars: TIdWideChars; ACharIndex: Integer): Integer;
var
  LBytes: PByte;
begin
  LBytes := ValidateBytes(ABytes, AByteIndex, AByteCount);
  if LBytes <> nil then begin
    Result := GetChars(LBytes, AByteCount, VChars, ACharIndex);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetChars(const ABytes: PByte; AByteCount: Integer): TIdWideChars;
var
  Len: Integer;
begin
  Len := GetCharCount(ABytes, AByteCount);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetChars(ABytes, AByteCount, PIdWideChar(Result), Len);
  end;
end;

function TIdTextEncodingBase.GetChars(const ABytes: PByte; AByteCount: Integer;
  var VChars: TIdWideChars; ACharIndex: Integer): Integer;
var
  LCharCount: Integer;
begin
  if (ABytes = nil) and (AByteCount <> 0) then begin
    raise Exception.CreateRes(PResStringRec(@RSInvalidSourceArray));
  end;
  if AByteCount < 0 then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidCharCount), [AByteCount]);
  end;
  if (ACharIndex < 0) or (ACharIndex > Length(VChars)) then begin
    raise Exception.CreateResFmt(PResStringRec(@RSInvalidDestinationIndex), [ACharIndex]);
  end;
  LCharCount := GetCharCount(ABytes, AByteCount);
  if LCharCount > 0 then begin
    if (ACharIndex + LCharCount) > Length(VChars) then begin
      raise Exception.CreateRes(PResStringRec(@RSInvalidDestinationArray));
    end;
    Result := GetChars(ABytes, AByteCount, @VChars[ACharIndex], LCharCount);
  end else begin
    Result := 0;
  end;
end;

function TIdTextEncodingBase.GetIsSingleByte: Boolean;
begin
  Result := FIsSingleByte;
end;

function TIdTextEncodingBase.GetPreamble: TIdBytes;
begin
  SetLength(Result, 0);
end;

function TIdTextEncodingBase.GetString(const ABytes: TIdBytes): TIdUnicodeString;
begin
  if ABytes <> nil then begin
    Result := GetString(PByte(ABytes), Length(ABytes));
  end else begin
    Result := '';
  end;
end;

function TIdTextEncodingBase.GetString(const ABytes: TIdBytes;
  AByteIndex, AByteCount: Integer): TIdUnicodeString;
var
  Len: Integer;
begin
  Result := '';
  Len := GetCharCount(ABytes, AByteIndex, AByteCount);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetChars(@ABytes[AByteIndex], AByteCount, PIdWideChar(Result), Len);
  end;
end;

function TIdTextEncodingBase.GetString(const ABytes: PByte; AByteCount: Integer): TIdUnicodeString;
var
  Len: Integer;
begin
  Result := '';
  Len := GetCharCount(ABytes, AByteCount);
  if Len > 0 then begin
    SetLength(Result, Len);
    GetChars(ABytes, AByteCount, PIdWideChar(Result), Len);
  end;
end;

{ TIdMBCSEncoding }

constructor TIdMBCSEncoding.Create;
begin
  {$IFDEF USE_ICONV}
  Create(iif(GIdIconvUseLocaleDependantAnsiEncoding, 'char', 'ASCII')); {do not localize}
  {$ELSE}
    {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
  Create(CP_ACP, 0, 0);
    {$ELSE}
  ToDo('Constructor of TIdMBCSEncoding class is not implemented for this platform yet'); {do not localize}
    {$ENDIF}
 {$ENDIF}
end;

{$IFDEF USE_ICONV}
constructor TIdMBCSEncoding.Create(const CharSet: String);
const
  // RLebeau: iconv() does not provide a maximum character byte size like
  // Microsoft does, so have to determine the max bytes by manually encoding
  // an actual Unicode codepoint.  We'll encode the largest codepoint that
  // UTF-16 supports, U+10FFFF, for now...
  //
  cValue: array[0..3] of Byte = ({$IFDEF ENDIAN_BIG}$DB, $FF, $DF, $FF{$ELSE}$FF, $DB, $FF, $DF{$ENDIF});
  //cValue: array[0..1] of UInt16 = ($DBFF, $DFFF);
begin
  inherited Create;

  // RLebeau 5/2/2017: have seen some malformed emails that use 'utf8'
  // instead of 'utf-8', so let's check for that...

  // RLebeau 9/27/2017: updating to handle a few more UTFs without hyphens...

  case PosInStrArray(CharSet, ['UTF7', 'UTF8', 'UTF16', 'UTF16LE', 'UTF16BE', 'UTF32', 'UTF32LE', 'UTF32BE'], False) of {Do not Localize}
    0:   FCharSet := 'UTF-7';    {Do not Localize}
    1:   FCharSet := 'UTF-8';    {Do not Localize}
    2,3: FCharSet := 'UTF-16LE'; {Do not Localize}
    4:   FCharSet := 'UTF-16BE'; {Do not Localize}
    5,6: FCharSet := 'UTF-32LE'; {Do not Localize}
    7:   FCharSet := 'UTF-32BE'; {Do not Localize}
  else
    FCharSet := CharSet;
  end;

  FMaxCharSize := GetByteCount(PIdWideChar(@cValue[0]), 2);

  // Not all charsets support all codepoints.  For example, ISO-8859-1 does
  // not support U+10FFFF.  If GetByteCount() fails above, FMaxCharSize gets
  // set to 0, preventing any character conversions.  So force FMaxCharSize
  // to 1 if GetByteCount() fails, until a better solution can be found.
  // Maybe loop through the codepoints until we find the largest one that is
  // supported by this charset..
  if FMaxCharSize = 0 then begin
    FMaxCharSize := 1;
  end;

  FIsSingleByte := (FMaxCharSize = 1);
end;
{$ELSE}
  {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
constructor TIdMBCSEncoding.Create(CodePage: Integer);
begin
  Create(CodePage, 0, 0);
end;

constructor TIdMBCSEncoding.Create(CodePage, MBToWCharFlags, WCharToMBFlags: Integer);
{$IFNDEF WINDOWS}
const
  // RLebeau: have to determine the max bytes by manually encoding an actual
  // Unicode codepoint.  We'll encode the largest codepoint that UTF-16 supports,
  // U+10FFFF, for now...
  //
  cValue: array[0..1] of UInt16 = ($DBFF, $DFFF);
{$ELSE}
var
  LCPInfo: TCPInfo;
  LError: Boolean;
{$ENDIF}
begin
  inherited Create;

  FCodePage := CodePage;
  FMBToWCharFlags := MBToWCharFlags;
  FWCharToMBFlags := WCharToMBFlags;

  {$IFDEF WINDOWS}
  LError := not GetCPInfo(FCodePage, LCPInfo);
  if LError and (FCodePage = 20127) then begin
    // RLebeau: 20127 is the official codepage for ASCII, but not
    // all OS versions support that codepage, so fallback to 1252
    // or even 437...
    FCodePage := 1252;
    LError := not GetCPInfo(FCodePage, LCPInfo);
    // just in case...
    if LError then begin
      FCodePage := 437;
      LError := not GetCPInfo(FCodePage, LCPInfo);
    end;
  end;
  if LError then begin
    raise EIdException.CreateResFmt(PResStringRec(@RSInvalidCodePage), [FCodePage]);
  end;
  FMaxCharSize := LCPInfo.MaxCharSize;
  {$ELSE}
  FMaxCharSize := LocaleCharsFromUnicode(FCodePage, FWCharToMBFlags, @cValue[0], 2, nil, 0, nil, nil);
  if FMaxCharSize < 1 then begin
    raise EIdException.CreateResFmt(@RSInvalidCodePage, [FCodePage]);
  end;
  // Not all charsets support all codepoints.  For example, ISO-8859-1 does
  // not support U+10FFFF.  If LocaleCharsFromUnicode() fails above,
  // FMaxCharSize gets set to 0, preventing any character conversions.  So
  // force FMaxCharSize to 1 if GetByteCount() fails, until a better solution
  // can be found.  Maybe loop through the codepoints until we find the largest
  // one that is supported by this codepage..
  if FMaxCharSize = 0 then begin
    FMaxCharSize := 1;
  end;
  {$ENDIF}
  FIsSingleByte := (FMaxCharSize = 1);
end;
  {$ENDIF}
{$ENDIF}

{$IFDEF USE_ICONV}
function CreateIconvHandle(const ACharSet: String; AToUTF16: Boolean): iconv_t;
const
  // RLebeau: iconv() outputs a UTF-16 BOM if data is converted to the generic
  // "UTF-16" charset.  We do not want that, so we will use the "UTF-16LE/BE"
  // charset explicitally instead so no BOM is outputted. This also saves us
  // from having to manually detect the presense of a BOM and strip it out.
  //
  // TODO: should we be using UTF-16LE or UTF-16BE on big-endian systems?
  // Delphi uses UTF-16LE, but what does FreePascal use? Let's err on the
  // side of caution until we know otherwise...
  //
  cUTF16CharSet = {$IFDEF ENDIAN_BIG}'UTF-16BE'{$ELSE}'UTF-16LE'{$ENDIF}; {do not localize}
var
  LToCharSet, LFromCharSet, LFlags: String;
  {$IFDEF USE_MARSHALLED_PTRS}
  M: TMarshaller;
  {$ENDIF}
begin
  // on some systems, //IGNORE must be specified before //TRANSLIT if they
  // are used together, otherwise //IGNORE gets ignored!
  LFlags := '';
  if GIdIconvIgnoreIllegalChars then begin
    LFlags := LFlags + '//IGNORE'; {do not localize}
  end;
  if GIdIconvUseTransliteration then begin
    LFlags := LFlags + '//TRANSLIT'; {do not localize}
  end;

  if AToUTF16 then begin
    LToCharSet := cUTF16CharSet + LFlags;
    LFromCharSet := ACharSet;
  end else begin
    LToCharSet := ACharSet + LFlags;
    LFromCharSet := cUTF16CharSet;
  end;

  Result := iconv_open(
    {$IFDEF USE_MARSHALLED_PTRS}
    M.AsAnsi(LToCharSet).ToPointer,
    M.AsAnsi(LFromCharSet).ToPointer
    {$ELSE}
    PAnsiChar(
      {$IFDEF STRING_IS_ANSI}
      LToCharSet
      {$ELSE}
      AnsiString(LToCharSet) // explicit convert to Ansi
      {$ENDIF}
    ),
    PAnsiChar(
      {$IFDEF STRING_IS_ANSI}
      LFromCharSet
      {$ELSE}
      AnsiString(LFromCharSet) // explicit convert to Ansi
      {$ENDIF}
    )
    {$ENDIF}
  );
  if Result = iconv_t(-1) then begin
    if LFlags <> '' then begin
      raise EIdException.CreateResFmt(@RSInvalidCharSetConvWithFlags, [ACharSet, cUTF16CharSet, LFlags]);
    end else begin
      raise EIdException.CreateResFmt(@RSInvalidCharSetConv, [ACharSet, cUTF16CharSet]);
    end;
  end;
end;

function CalcUTF16ByteSize(AChars: PWideChar; ACharCount: Integer): Integer;
var
  C: WideChar;
  LCount: Integer;
begin
  C := AChars^;
  if (C >= #$D800) and (C <= #$DFFF) then
  begin
    Result := 0;
    if C > #$DBFF then begin
      // invalid high surrogate
      Exit;
    end;
    if ACharCount = 1 then begin
      // missing low surrogate
      Exit;
    end;
    Inc(AChars);
    C := AChars^;
    if (C < #$DC00) or (C > #$DFFF) then begin
      // invalid low surrogate
      Exit;
    end;
    LCount := 2;
  end else begin
    LCount := 1;
  end;
  Result := LCount * SizeOf(WideChar);
end;
{$ENDIF}

{$IFDEF USE_ICONV}
function DoIconvCharsToBytes(const ACharset: string; AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer; ABytesIsTemp: Boolean): Integer;
var
  LSrcCharsPtr: PIdWideChar;
  LCharsPtr, LBytesPtr: PAnsiChar;
  LSrcCharSize, LCharSize, LByteSize: size_t;
  LCharsRead, LBytesWritten: Integer;
  LIconv: iconv_t;
begin
  Result := 0;

  if (AChars = nil) or (ACharCount = 0) then begin
    Exit;
  end;

  LIconv := CreateIconvHandle(ACharSet, False);
  try
    // RLebeau: iconv() does not allow for querying a pre-calculated byte size
    // for the input like Microsoft does, so have to determine the max bytes
    // by actually encoding the Unicode data to a real buffer.  When ABytesIsTemp
    // is True, we are encoding to a small local buffer so we don't have to use
    // a lot of memory. We also have to encode the input 1 Unicode codepoint at
    // a time to avoid iconv() returning an E2BIG error if multiple UTF-16
    // sequences were decoded to a length that would exceed the size of the
    // local buffer.

    //Kylix has an odd definition in iconv.  In Kylix, __outbytesleft is defined as a var
    //while in FreePascal's libc and our IdIconv units define it as a pSize_t

    // reset to initial state
    LByteSize := 0;
    if iconv(LIconv, nil, nil, nil, {$IFNDEF KYLIX}@{$ENDIF}LByteSize) = size_t(-1) then begin
      Exit;
    end;

    // do the conversion
    LSrcCharsPtr := AChars;
    repeat
      if LSrcCharsPtr <> nil then begin
        LSrcCharSize := CalcUTF16ByteSize(LSrcCharsPtr, ACharCount);
        if LSrcCharSize = 0 then begin
          Result := 0;
          Exit;
        end;
      end else begin
        LSrcCharSize := 0;
      end;

      LCharsPtr := PAnsiChar(LSrcCharsPtr);
      LCharSize := LSrcCharSize;
      LBytesPtr := PAnsiChar(ABytes);
      LByteSize := AByteCount;
      if iconv(LIconv, @LCharsPtr, @LCharSize, @LBytesPtr, {$IFNDEF KYLIX}@{$ENDIF}LByteSize) = size_t(-1) then
      begin
        Exit;
      end;

      // LByteSize was decremented by the number of bytes stored in the output buffer
      LBytesWritten := AByteCount - LByteSize;
      Inc(Result, LBytesWritten);
      if LSrcCharsPtr = nil then begin
        Exit;
      end;

      if not ABytesIsTemp then begin
        Inc(ABytes, LBytesWritten);
        Dec(AByteCount, LBytesWritten);
      end;

      // LCharSize was decremented by the number of bytes read from the input buffer
      LCharsRead := (LSrcCharSize-LCharSize) div SizeOf(WideChar);
      Inc(LSrcCharsPtr, LCharsRead);
      Dec(ACharCount, LCharsRead);
      if ACharCount < 1 then
      begin
        // After all characters are handled, the output buffer has to be flushed
        // This is done by running one more iteration, without an input buffer
        LSrcCharsPtr := nil;
      end;
    until False;
  finally
    iconv_close(LIconv);
  end;
end;
{$ENDIF}

function TIdMBCSEncoding.GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer;
{$IFDEF USE_ICONV}
var
  LBytes: array[0..3] of Byte;
{$ENDIF}
begin
  {$IFDEF USE_ICONV}
  Result := DoIconvCharsToBytes(FCharset, AChars, ACharCount, @LBytes[0], Length(LBytes), True);
  {$ELSE}
    {$IFDEF HAS_LocaleCharsFromUnicode}
  Result := LocaleCharsFromUnicode(FCodePage, FWCharToMBFlags, AChars, ACharCount, nil, 0, nil, nil);
    {$ELSE}
      {$IFDEF WINDOWS}
  Result := WideCharToMultiByte(FCodePage, FWCharToMBFlags, AChars, ACharCount, nil, 0, nil, nil);
      {$ELSE}
  Result := 0;
  ToDo('GetByteCount() method of TIdMBCSEncoding class is not implemented for this platform yet'); {do not localize}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TIdMBCSEncoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer; ABytes: PByte;
  AByteCount: Integer): Integer;
begin
  {$IFDEF USE_ICONV}
  Assert (ABytes <> nil, 'TIdMBCSEncoding.GetBytes Bytes can not be nil');
  Result := DoIconvCharsToBytes(FCharset, AChars, ACharCount, ABytes, AByteCount, False);
  {$ELSE}
    {$IFDEF HAS_LocaleCharsFromUnicode}
  Result := LocaleCharsFromUnicode(FCodePage, FWCharToMBFlags, AChars, ACharCount, {$IFNDEF HAS_PAnsiChar}Pointer{$ELSE}PAnsiChar{$ENDIF}(ABytes), AByteCount, nil, nil);
    {$ELSE}
      {$IFDEF WINDOWS}
  Result := WideCharToMultiByte(FCodePage, FWCharToMBFlags, AChars, ACharCount, PAnsiChar(ABytes), AByteCount, nil, nil);
      {$ELSE}
  Result := 0;
  ToDo('GetBytes() method of TIdMBCSEncoding class is not implemented for this platform yet'); {do not localize}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

{$IFDEF USE_ICONV}
function DoIconvBytesToChars(const ACharset: string; const ABytes: PByte; AByteCount: Integer;
  AChars: PWideChar; ACharCount: Integer; AMaxCharSize: Integer; ACharsIsTemp: Boolean): Integer;
var
  LSrcBytesPtr: PByte;
  LBytesPtr, LCharsPtr: PAnsiChar;
  LByteSize, LCharsSize: size_t;
  I, LDestCharSize, LMaxBytesSize, LBytesRead, LCharsWritten: Integer;
  LConverted: Boolean;
  LIconv: iconv_t;
begin
  Result := 0;

  if (ABytes = nil) or (AByteCount = 0) then begin
    Exit;
  end;

  LIconv := CreateIconvHandle(ACharset, True);
  try
    // RLebeau: iconv() does not allow for querying a pre-calculated character count
    // for the input like Microsoft does, so have to determine the max characters
    // by actually encoding the Ansi data to a real buffer.  If ACharsIsTemp is True
    // then we are encoding to a small local buffer so we don't have to use a lot of
    // memory.  We also have to encode the input 1 Unicode codepoint at a time to
    // avoid iconv() returning an E2BIG error if multiple MBCS sequences were decoded
    // to a length that would exceed the size of the local buffer.

    //Kylix has an odd definition in iconv.  In Kylix, __outbytesleft is defined as a var
    //while in FreePascal's libc and our IdIconv units define it as a pSize_t

    // reset to initial state
    LCharsSize := 0;
    if iconv(LIconv, nil, nil, nil, {$IFNDEF KYLIX}@{$ENDIF}LCharsSize) = size_t(-1) then
    begin
      Exit;
    end;

    // do the conversion
    LSrcBytesPtr := ABytes;
    repeat
      LMaxBytesSize := IndyMin(AByteCount, AMaxCharSize);
      LDestCharSize := ACharCount * SizeOf(WideChar);

      if LSrcBytesPtr = nil then
      begin
        LBytesPtr := nil;
        LByteSize := 0;
        LCharsPtr := PAnsiChar(AChars);
        LCharsSize := LDestCharSize;
        if iconv(LIconv, @LBytesPtr, @LByteSize, @LCharsPtr, {$IFNDEF KYLIX}@{$ENDIF}LCharsSize) = size_t(-1) then
        begin
          Result := 0;
        end else
        begin
          // LCharsSize was decremented by the number of bytes stored in the output buffer
          Inc(Result, (LDestCharSize-LCharsSize) div SizeOf(WideChar));
        end;
        Exit;
      end;

      // TODO: figure out a better way to calculate the number of input bytes
      // needed to generate a single UTF-16 output sequence...
      LMaxBytesSize := IndyMin(AByteCount, AMaxCharSize);
      LConverted := False;
      for I := 1 to LMaxBytesSize do
      begin
        LBytesPtr := PAnsiChar(LSrcBytesPtr);
        LByteSize := I;
        LCharsPtr := PAnsiChar(AChars);
        LCharsSize := LDestCharSize;
        if iconv(LIconv, @LBytesPtr, @LByteSize, @LCharsPtr, {$IFNDEF KYLIX}@{$ENDIF}LCharsSize) <> size_t(-1) then
        begin
          LConverted := True;

          // LCharsSize was decremented by the number of bytes stored in the output buffer
          LCharsWritten := (LDestCharSize-LCharsSize) div SizeOf(WideChar);
          Inc(Result, LCharsWritten);
          if LSrcBytesPtr = nil then begin
            Exit;
          end;

          if not ACharsIsTemp then begin
            Inc(AChars, LCharsWritten);
            Dec(ACharCount, LCharsWritten);
          end;

          // LByteSize was decremented by the number of bytes read from the input buffer
          LBytesRead := I - LByteSize;
          Inc(LSrcBytesPtr, LBytesRead);
          Dec(AByteCount, LBytesRead);
          if AByteCount < 1 then begin
            // After all bytes are handled, the output buffer has to be flushed
            // This is done by running one more iteration, without an input buffer
            LSrcBytesPtr := nil;
          end;

          Break;
        end;
      end;

      if not LConverted then begin
        Result := 0;
        Exit;
      end;
    until False;
  finally
    iconv_close(LIconv);
  end;
end;
{$ENDIF}

function TIdMBCSEncoding.GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer;
{$IFDEF USE_ICONV}
var
  LChars: array[0..3] of WideChar;
{$ENDIF}
begin
  {$IFDEF USE_ICONV}
  Result := DoIconvBytesToChars(FCharSet, ABytes, AByteCount, @LChars[0], Length(LChars), FMaxCharSize, True);
  {$ELSE}
    {$IFDEF HAS_UnicodeFromLocaleChars}
  Result := UnicodeFromLocaleChars(FCodePage, FMBToWCharFlags, {$IFNDEF HAS_PAnsiChar}Pointer{$ELSE}PAnsiChar{$ENDIF}(ABytes), AByteCount, nil, 0);
    {$ELSE}
      {$IFDEF WINDOWS}
  Result := MultiByteToWideChar(FCodePage, FMBToWCharFlags, PAnsiChar(ABytes), AByteCount, nil, 0);
      {$ELSE}
  Result := 0;
  ToDo('GetCharCount() method of TIdMBCSEncoding class is not implemented for this platform yet'); {do not localize}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TIdMBCSEncoding.GetChars(const ABytes: PByte; AByteCount: Integer; AChars: PWideChar;
  ACharCount: Integer): Integer;
begin
  {$IFDEF USE_ICONV}
  Result := DoIconvBytesToChars(FCharSet, ABytes, AByteCount, AChars, ACharCount, FMaxCharSize, False);
  {$ELSE}
    {$IFDEF HAS_UnicodeFromLocaleChars}
  Result := UnicodeFromLocaleChars(FCodePage, FMBToWCharFlags, {$IFNDEF HAS_PAnsiChar}Pointer{$ELSE}PAnsiChar{$ENDIF}(ABytes), AByteCount, AChars, ACharCount);
    {$ELSE}
      {$IFDEF WINDOWS}
  Result := MultiByteToWideChar(FCodePage, FMBToWCharFlags, PAnsiChar(ABytes), AByteCount, AChars, ACharCount);
      {$ELSE}
  Result := 0;
  ToDo('GetChars() method of TIdMBCSEncoding class is not implemented for this platform yet'); {do not localize}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TIdMBCSEncoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * FMaxCharSize;
end;

function TIdMBCSEncoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

function TIdMBCSEncoding.GetPreamble: TIdBytes;
begin
  {$IFDEF USE_ICONV}
  // RLebeau 5/2/2017: have seen some malformed emails that use 'utf8'
  // instead of 'utf-8', so let's check for that...

  // RLebeau 9/27/2017: updating to handle a few more UTFs without hyphens...

  case PosInStrArray(FCharSet, ['UTF-8', 'UTF8', 'UTF-16', 'UTF16', 'UTF-16LE', 'UTF16LE', 'UTF-16BE', 'UTF16BE', 'UTF-32', 'UTF32', 'UTF-32LE', 'UTF32LE', 'UTF-32BE', 'UTF32BE'], False) of {do not localize}
    0, 1: begin
      SetLength(Result, 3);
      Result[0] := $EF;
      Result[1] := $BB;
      Result[2] := $BF;
    end;
    2..5: begin
      SetLength(Result, 2);
      Result[0] := $FF;
      Result[1] := $FE;
    end;
    6, 7: begin
      SetLength(Result, 2);
      Result[0] := $FE;
      Result[1] := $FF;
    end;
    8..11: begin
      SetLength(Result, 4);
      Result[0] := $FF;
      Result[1] := $FE;
      Result[2] := $00;
      Result[3] := $00;
    end;
    12, 13: begin
      SetLength(Result, 4);
      Result[0] := $00;
      Result[1] := $00;
      Result[2] := $FE;
      Result[3] := $FF;
    end;
  else
    SetLength(Result, 0);
  end;
  {$ELSE}
    {$IFDEF WINDOWS}
  case FCodePage of
    CP_UTF8: begin
      SetLength(Result, 3);
      Result[0] := $EF;
      Result[1] := $BB;
      Result[2] := $BF;
    end;
    1200: begin
      SetLength(Result, 2);
      Result[0] := $FF;
      Result[1] := $FE;
    end;
    1201: begin
      SetLength(Result, 2);
      Result[0] := $FE;
      Result[1] := $FF;
    end;
    12000: begin
      SetLength(Result, 4);
      Result[0] := $FF;
      Result[1] := $FE;
      Result[2] := $00;
      Result[3] := $00;
    end;
    12001: begin
      SetLength(Result, 4);
      Result[0] := $00;
      Result[1] := $00;
      Result[2] := $FE;
      Result[3] := $FF;
    end;
  else
    SetLength(Result, 0);
  end;
    {$ELSE}
  SetLength(Result, 0);
  ToDo('GetPreamble() method of TIdMBCSEncoding class is not implemented for this platform yet'); {do not localize}
    {$ENDIF}
  {$ENDIF}
end;

{ TIdUTF7Encoding }

constructor TIdUTF7Encoding.Create;
begin
  {$IFDEF USE_ICONV}
  inherited Create('UTF-7'); {do not localize}
  {$ELSE}
    {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
  inherited Create(CP_UTF7);
    {$ELSE}
  ToDo('Construtor of TIdUTF7Encoding class is not implemented for this platform yet'); {do not localize}
    {$ENDIF}
  {$ENDIF}
end;

function TIdUTF7Encoding.GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  Result := inherited GetByteCount(AChars, ACharCount);
end;

function TIdUTF7Encoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer): Integer;
begin
  Result := inherited GetBytes(AChars, ACharCount, ABytes, AByteCount);
end;

function TIdUTF7Encoding.GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer;
begin
  Result := inherited GetCharCount(ABytes, AByteCount);
end;

function TIdUTF7Encoding.GetChars(const ABytes: PByte; AByteCount: Integer;
  AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  Result := inherited GetChars(ABytes, AByteCount, AChars, ACharCount);
end;

function TIdUTF7Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount * 3) + 2;
end;

function TIdUTF7Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

{ TIdUTF8Encoding }

// TODO: implement UTF-8 manually so we don't have to deal with codepage issues...

constructor TIdUTF8Encoding.Create;
begin
  {$IFDEF USE_ICONV}
  inherited Create('UTF-8'); {do not localize}
  {$ELSE}
    {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
  inherited Create(CP_UTF8, 0, 0);
    {$ELSE}
  ToDo('Constructor of TIdUTF8Encoding class is not implemented for this platform yet'); {do not localize}
    {$ENDIF}
  {$ENDIF}
end;

function TIdUTF8Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 3;
end;

function TIdUTF8Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount + 1;
end;

function TIdUTF8Encoding.GetPreamble: TIdBytes;
begin
  SetLength(Result, 3);
  Result[0] := $EF;
  Result[1] := $BB;
  Result[2] := $BF;
end;

{ TIdUTF16LittleEndianEncoding }

constructor TIdUTF16LittleEndianEncoding.Create;
begin
  inherited Create;
  FIsSingleByte := False;
  FMaxCharSize := 4;
end;

function TIdUTF16LittleEndianEncoding.GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  // TODO: verify UTF-16 sequences
  Result := ACharCount * SizeOf(WideChar);
end;

function TIdUTF16LittleEndianEncoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer): Integer;
{$IFDEF ENDIAN_BIG}
var
  I: Integer;
  LChars: PIdWideChar;
{$ENDIF}
begin
  // TODO: verify UTF-16 sequences
  {$IFDEF ENDIAN_BIG}
  LChars := AChars;
  for I := ACharCount - 1 downto 0 do
  begin
    ABytes^ := Hi(UInt16(LChars^));
    Inc(ABytes);
    ABytes^ := Lo(UInt16(LChars^));
    Inc(ABytes);
    Inc(LChars);
  end;
  Result := ACharCount * SizeOf(WideChar);
  {$ELSE}
  Result := ACharCount * SizeOf(WideChar);
  Move(AChars^, ABytes^, Result);
  {$ENDIF}
end;

function TIdUTF16LittleEndianEncoding.GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer;
begin
  // TODO: verify UTF-16 sequences
  Result := AByteCount div SizeOf(WideChar);
end;

function TIdUTF16LittleEndianEncoding.GetChars(const ABytes: PByte; AByteCount: Integer;
  AChars: PIdWideChar; ACharCount: Integer): Integer;
{$IFDEF ENDIAN_BIG}
var
  LBytes1, LBytes2: PByte;
  I: Integer;
{$ENDIF}
begin
  // TODO: verify UTF-16 sequences
  {$IFDEF ENDIAN_BIG}
  LBytes1 := ABytes;
  LBytes2 := ABytes;
  Inc(LBytes2);
  for I := 0 to ACharCount - 1 do
  begin
    AChars^ := WideChar(MakeWord(LBytes2^, LBytes1^));
    Inc(LBytes1, 2);
    Inc(LBytes2, 2);
    Inc(AChars);
  end;
  Result := ACharCount;
  {$ELSE}
  Result := AByteCount div SizeOf(WideChar);
  Move(ABytes^, AChars^, Result * SizeOf(WideChar));
  {$ENDIF}
end;

function TIdUTF16LittleEndianEncoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 2;
end;

function TIdUTF16LittleEndianEncoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := (ByteCount div SizeOf(WideChar)) + (ByteCount and 1) + 1;
end;

function TIdUTF16LittleEndianEncoding.GetPreamble: TIdBytes;
begin
  SetLength(Result, 2);
  Result[0] := $FF;
  Result[1] := $FE;
end;

{ TIdUTF16BigEndianEncoding }

function TIdUTF16BigEndianEncoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer): Integer;
{$IFDEF ENDIAN_LITTLE}
var
  I: Integer;
  P: PIdWideChar;
{$ENDIF}
begin
  {$IFDEF ENDIAN_LITTLE}
  P := AChars;
  for I := ACharCount - 1 downto 0 do
  begin
    ABytes^ := Hi(UInt16(P^));
    Inc(ABytes);
    ABytes^ := Lo(UInt16(P^));
    Inc(ABytes);
    Inc(P);
  end;
  Result := ACharCount * SizeOf(WideChar);
  {$ELSE}
  Result := ACharCount * SizeOf(WideChar);
  Move(AChars^, ABytes^, Result);
  {$ENDIF}
end;

function TIdUTF16BigEndianEncoding.GetChars(const ABytes: PByte; AByteCount: Integer;
  AChars: PIdWideChar; ACharCount: Integer): Integer;
{$IFDEF ENDIAN_LITTLE}
var
  P1, P2: PByte;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF ENDIAN_LITTLE}
  P1 := ABytes;
  P2 := P1;
  Inc(P1);
  for I := 0 to ACharCount - 1 do
  begin
    AChars^ := WideChar(MakeWord(P1^, P2^));
    Inc(P2, 2);
    Inc(P1, 2);
    Inc(AChars);
  end;
  Result := ACharCount;
  {$ELSE}
  Result := AByteCount div SizeOf(WideChar);
  Move(ABytes^, AChars^, Result * SizeOf(WideChar));
  {$ENDIF}
end;

function TIdUTF16BigEndianEncoding.GetPreamble: TIdBytes;
begin
  SetLength(Result, 2);
  Result[0] := $FE;
  Result[1] := $FF;
end;

{ TIdASCIIEncoding }

function IsCharsetASCII(const ACharSet: string): Boolean;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // TODO: when the IdCharsets unit is moved to the System
  // package, use CharsetToCodePage() here...
  Result := PosInStrArray(ACharSet,
      [
      'US-ASCII',                                      {do not localize}
      'ANSI_X3.4-1968',                                {do not localize}
      'iso-ir-6',                                      {do not localize}
      'ANSI_X3.4-1986',                                {do not localize}
      'ISO_646.irv:1991',                              {do not localize}
      'ASCII',                                         {do not localize}
      'ISO646-US',                                     {do not localize}
      'us',                                            {do not localize}
      'IBM367',                                        {do not localize}
      'cp367',                                         {do not localize}
      'csASCII'                                        {do not localize}
      ], False) <> -1;
end;

constructor TIdASCIIEncoding.Create;
begin
  inherited Create;
  FIsSingleByte := True;
  FMaxCharSize := 1;
end;

function TIdASCIIEncoding.GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  Result := ACharCount;
end;

function TIdASCIIEncoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer): Integer;
var
  P: PIdWideChar;
  i : Integer;
begin
  P := AChars;
  Result := IndyMin(ACharCount, AByteCount);
  for i := 1 to Result do begin
    // replace illegal characters > $7F
    if UInt16(P^) > $007F then begin
      ABytes^ := Byte(Ord('?'));
    end else begin
      ABytes^ := Byte(P^);
    end;
    //advance to next char
    Inc(P);
    Inc(ABytes);
  end;
end;

function TIdASCIIEncoding.GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer;
begin
  Result := AByteCount;
end;

function TIdASCIIEncoding.GetChars(const ABytes: PByte; AByteCount: Integer;
  AChars: PIdWideChar; ACharCount: Integer): Integer;
var
  P: PByte;
  i : Integer;
begin
  P := ABytes;
  Result := IndyMin(ACharCount, AByteCount);
  for i := 1 to Result do begin
    // This is an invalid byte in the ASCII encoding.
    if P^ > $7F then begin
      UInt16(AChars^) := $FFFD;
    end else begin
      UInt16(AChars^) := P^;
    end;
    //advance to next byte
    Inc(AChars);
    Inc(P);
  end;
end;

function TIdASCIIEncoding.GetMaxByteCount(ACharCount: Integer): Integer;
begin
  Result := ACharCount;
end;

function TIdASCIIEncoding.GetMaxCharCount(AByteCount: Integer): Integer;
begin
  Result := AByteCount;
end;

{ TId8BitEncoding }

constructor TId8BitEncoding.Create;
begin
  inherited Create;
  FIsSingleByte := True;
  FMaxCharSize := 1;
end;

function TId8BitEncoding.GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  Result := ACharCount;
end;

function TId8BitEncoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer): Integer;
var
  P: PIdWideChar;
  i : Integer;
begin
  P := AChars;
  Result := IndyMin(ACharCount, AByteCount);
  for i := 1 to Result do begin
    // replace illegal characters > $FF
    if UInt16(P^) > $00FF then begin
      ABytes^ := Byte(Ord('?'));
    end else begin
      ABytes^ := Byte(P^);
    end;
    //advance to next char
    Inc(P);
    Inc(ABytes);
  end;
end;

function TId8BitEncoding.GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer;
begin
  Result := AByteCount;
end;

function TId8BitEncoding.GetChars(const ABytes: PByte; AByteCount: Integer;
  AChars: PIdWideChar; ACharCount: Integer): Integer;
var
  P: PByte;
  i : Integer;
begin
  P := ABytes;
  Result := IndyMin(ACharCount, AByteCount);
  for i := 1 to Result do begin
    UInt16(AChars^) := P^;
    //advance to next char
    Inc(AChars);
    Inc(P);
  end;
end;

function TId8BitEncoding.GetMaxByteCount(ACharCount: Integer): Integer;
begin
  Result := ACharCount;
end;

function TId8BitEncoding.GetMaxCharCount(AByteCount: Integer): Integer;
begin
  Result := AByteCount;
end;

{ TIdVCLEncoding }

{$IFDEF HAS_TEncoding}

// RLebeau: this is a hack.  The protected members of SysUtils.TEncoding are
// declared as 'STRICT protected', so a regular accessor will not work here.
// Only descendants can call them, so we have to expose our own methods that
// this unit can call, and have them call the inherited methods internally.

type
  TEncodingAccess = class(TEncoding)
  public
    function IndyGetByteCount(Chars: PChar; CharCount: Integer): Integer;
    function IndyGetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
    function IndyGetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
    function IndyGetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
  end;

function TEncodingAccess.IndyGetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
  Result := GetByteCount(Chars, CharCount);
end;

function TEncodingAccess.IndyGetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetBytes(Chars, CharCount, Bytes, ByteCount);
end;

function TEncodingAccess.IndyGetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetCharCount(Bytes, ByteCount);
end;

function TEncodingAccess.IndyGetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PChar; CharCount: Integer): Integer;
begin
  Result := GetChars(Bytes, ByteCount, Chars, CharCount);
end;

constructor TIdVCLEncoding.Create(AEncoding: TEncoding; AFreeEncoding: Boolean);
begin
  inherited Create;
  FEncoding := AEncoding;
  FFreeEncoding := AFreeEncoding and not TEncoding.IsStandardEncoding(AEncoding);
  FIsSingleByte := FEncoding.IsSingleByte;
end;

{$IFDEF HAS_TEncoding_GetEncoding_ByEncodingName}
constructor TIdVCLEncoding.Create(const ACharset: String);
var
  LCharset: string;
begin
  // RLebeau 5/2/2017: have seen some malformed emails that use 'utf8'
  // instead of 'utf-8', so let's check for that...

  // RLebeau 9/27/2017: updating to handle a few more UTFs without hyphens...

  case PosInStrArray(ACharset, ['UTF7', 'UTF8', 'UTF16', 'UTF16LE', 'UTF16BE', 'UTF32', 'UTF32LE', 'UTF32BE'], False) of {Do not Localize}
    0:   LCharset := 'UTF-7';    {Do not Localize}
    1:   LCharset := 'UTF-8';    {Do not Localize}
    2,3: LCharset := 'UTF-16LE'; {Do not Localize}
    4:   LCharset := 'UTF-16BE'; {Do not Localize}
    5,6: LCharset := 'UTF-32LE'; {Do not Localize}
    7:   LCharset := 'UTF-32BE'; {Do not Localize}
  else
    LCharset := ACharset;
  end;

  Create(TEncoding.GetEncoding(LCharset), True);
end;
{$ENDIF}

constructor TIdVCLEncoding.Create(const ACodepage: UInt16);
begin
  Create(TEncoding.GetEncoding(ACodepage), True);
end;

destructor TIdVCLEncoding.Destroy;
begin
  if FFreeEncoding then begin
    FEncoding.Free;
  end;
  inherited Destroy;
end;

function TIdVCLEncoding.GetByteCount(const AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  Result := TEncodingAccess(FEncoding).IndyGetByteCount(AChars, ACharCount);
end;

function TIdVCLEncoding.GetBytes(const AChars: PIdWideChar; ACharCount: Integer;
  ABytes: PByte; AByteCount: Integer): Integer;
begin
  Result := TEncodingAccess(FEncoding).IndyGetBytes(AChars, ACharCount, ABytes, AByteCount);
end;

function TIdVCLEncoding.GetCharCount(const ABytes: PByte; AByteCount: Integer): Integer;
begin
  Result := TEncodingAccess(FEncoding).IndyGetCharCount(ABytes, AByteCount);
end;

function TIdVCLEncoding.GetChars(const ABytes: PByte; AByteCount: Integer;
  AChars: PIdWideChar; ACharCount: Integer): Integer;
begin
  Result := TEncodingAccess(FEncoding).IndyGetChars(ABytes, AByteCount, AChars, ACharCount);
end;

function TIdVCLEncoding.GetMaxByteCount(ACharCount: Integer): Integer;
begin
  Result := FEncoding.GetMaxByteCount(ACharCount);
end;

function TIdVCLEncoding.GetMaxCharCount(AByteCount: Integer): Integer;
begin
  Result := FEncoding.GetMaxCharCount(AByteCount);
end;
{$ENDIF}

{$ENDIF}

function IndyTextEncoding(AType: IdTextEncodingType): IIdTextEncoding;
begin
  case AType of
    encIndyDefault: Result := IndyTextEncoding_Default;
    // encOSDefault handled further below
    enc8Bit:        Result := IndyTextEncoding_8Bit;
    encASCII:       Result := IndyTextEncoding_ASCII;
    encUTF16BE:     Result := IndyTextEncoding_UTF16BE;
    encUTF16LE:     Result := IndyTextEncoding_UTF16LE;
    encUTF7:        Result := IndyTextEncoding_UTF7;
    encUTF8:        Result := IndyTextEncoding_UTF8;
  else
    // encOSDefault
    Result := IndyTextEncoding_OSDefault;
  end;
end;

function IndyTextEncoding(ACodepage: UInt16): IIdTextEncoding;
begin
  {$IFDEF DOTNET}
  Result := TIdDotNetEncoding.Create(ACodepage);
  {$ELSE}
  case ACodepage of
    20127:
      Result := IndyTextEncoding_ASCII;
    1200:
      Result := IndyTextEncoding_UTF16LE;
    1201:
      Result := IndyTextEncoding_UTF16BE;
    65000:
      Result := IndyTextEncoding_UTF7;
    65001:
      Result := IndyTextEncoding_UTF8;
    // TODO: add support for UTF-32...
  else
    {$IFDEF SUPPORTS_CODEPAGE_ENCODING}
    Result := TIdMBCSEncoding.Create(ACodepage);
    {$ELSE}
      {$IFDEF HAS_TEncoding}
    Result := TIdVCLEncoding.Create(ACodepage);
      {$ELSE}
    Result := nil;
    raise EIdException.CreateResFmt(@RSUnsupportedCodePage, [ACodepage]);
      {$ENDIF}
    {$ENDIF}
  end;
  {$ENDIF}
end;

function IndyTextEncoding(const ACharSet: String): IIdTextEncoding;
begin
  {$IFDEF DOTNET}
  Result := TIdDotNetEncoding.Create(ACharSet);
  {$ELSE}
  // TODO: move IdCharsets unit into the System package so the
  // IdGlobalProtocols.CharsetToEncoding() function can be moved
  // into this unit...
  if IsCharsetASCII(ACharSet) then begin
    Result := IndyTextEncoding_ASCII;
  end else begin
    // RLebeau 5/2/2017: have seen some malformed emails that use 'utf8'
    // instead of 'utf-8', so let's check for that...

    // RLebeau 9/27/2017: updating to handle a few more UTFs without hyphens...

    // TODO: add support for UTF-32...
    case PosInStrArray(ACharset, ['UTF-7', 'UTF7', 'UTF-8', 'UTF8', 'UTF-16', 'UTF16', 'UTF-16LE', 'UTF16LE', 'UTF-16BE', 'UTF16BE'], False) of {Do not Localize}
      0, 1: Result := IndyTextEncoding_UTF7;
      2, 3: Result := IndyTextEncoding_UTF8;
      4..7: Result := IndyTextEncoding_UTF16LE;
      8, 9: Result := IndyTextEncoding_UTF16BE;
      // TODO: add support for UTF-32...
    else
      {$IFDEF USE_ICONV}
      Result := TIdMBCSEncoding.Create(ACharSet);
      {$ELSE}
        {$IFDEF HAS_TEncoding_GetEncoding_ByEncodingName}
      Result := TIdVCLEncoding.Create(ACharSet);
        {$ELSE}
      // TODO: provide a hook that IdGlobalProtocols can assign to so we can call
      // CharsetToCodePage() here, at least until CharsetToEncoding() can be moved
      // to this unit once IdCharsets has been moved to the System package...
      Result := nil;
      raise EIdException.CreateResFmt(PResStringRec(@RSUnsupportedCharSet), [ACharSet]);
        {$ENDIF}
      {$ENDIF}
    end;
  end;
  {$ENDIF}
end;

{$IFDEF DOTNET}
function IndyTextEncoding(AEncoding: System.Text.Encoding): IIdTextEncoding;
begin
  Result := TIdDotNetEncoding.Create(AEncoding);
end;
{$ENDIF}

{$IFDEF HAS_TEncoding}
function IndyTextEncoding(AEncoding: TEncoding; AFreeEncoding: Boolean = False): IIdTextEncoding;
begin
  Result := TIdVCLEncoding.Create(AEncoding, AFreeEncoding);
end;
{$ENDIF}

function IndyTextEncoding_Default: IIdTextEncoding;
var
  LType: IdTextEncodingType;
begin
  LType := GIdDefaultTextEncoding;
  if LType = encIndyDefault then begin
    LType := encASCII;
  end;
  Result := IndyTextEncoding(LType);
end;

function IndyTextEncoding_OSDefault: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GIdOSDefaultEncoding = nil then begin
    {$IFDEF DOTNET}
    // TODO: use thread-safe assignment
    GIdOSDefaultEncoding := TIdDotNetEncoding.Create(System.Text.Encoding.Default);
    {$ELSE}
    // TODO: SysUtils.TEncoding.Default uses ANSI on Windows
    // but uses UTF-8 on POSIX, so we should do the same...
    //LEncoding := {$IFDEF WINDOWS}TIdMBCSEncoding{$ELSE}TIdUTF8Encoding{$ENDIF}.Create;
    LEncoding := TIdMBCSEncoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GIdOSDefaultEncoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GIdOSDefaultEncoding;
end;

function IndyTextEncoding_8Bit: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GId8BitEncoding = nil then begin
    {$IFDEF DOTNET}
    // We need a charset that converts UTF-16 codeunits in the $00-$FF range
    // to/from their numeric values as-is.  Was previously using "Windows-1252"
    // which does so for most codeunits, however codeunits $80-$9F in
    // Windows-1252 map to different codepoints in Unicode, which we don't want.
    // "ISO-8859-1" aka "ISO_8859-1:1987" (not to be confused with the older
    // "ISO 8859-1" charset), on the other hand, treats codeunits $00-$FF as-is,
    // and seems to be just as widely supported as Windows-1252 on most systems,
    // so we'll use that for now...

    // TODO: use thread-safe assignment
    GId8BitEncoding := TIdDotNetEncoding.Create('ISO-8859-1');
    {$ELSE}
    LEncoding := TId8BitEncoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GId8BitEncoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GId8BitEncoding;
end;

function IndyTextEncoding_ASCII: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GIdASCIIEncoding = nil then begin
    {$IFDEF DOTNET}
    // TODO: use thread-safe assignment
    GIdASCIIEncoding := TIdDotNetEncoding.Creeate(System.Text.Encoding.ASCII);
    {$ELSE}
    LEncoding := TIdASCIIEncoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GIdASCIIEncoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GIdASCIIEncoding;
end;

function IndyTextEncoding_UTF16BE: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GIdUTF16BigEndianEncoding = nil then begin
    {$IFDEF DOTNET}
    // TODO: use thread-safe assignment
    GIdUTF16BigEndianEncoding := TIdDotNetEncoding.Create(System.Text.Encoding.BigEndianUnicode);
    {$ELSE}
    LEncoding := TIdUTF16BigEndianEncoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GIdUTF16BigEndianEncoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GIdUTF16BigEndianEncoding;
end;

function IndyTextEncoding_UTF16LE: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GIdUTF16LittleEndianEncoding = nil then begin
    {$IFDEF DOTNET}
    // TODO: use thread-safe assignment
    GIdUTF16LittleEndianEncoding := TIdDotNetEncoding.Create(System.Text.Encoding.Unicode);
    {$ELSE}
    LEncoding := TIdUTF16LittleEndianEncoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GIdUTF16LittleEndianEncoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GIdUTF16LittleEndianEncoding;
end;

function IndyTextEncoding_UTF7: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GIdUTF7Encoding = nil then begin
    {$IFDEF DOTNET}
    // TODO: use thread-safe assignment
    GIdUTF7Encoding := TIdDotNetEncoding.Create(System.Text.Encoding.UTF7);
    {$ELSE}
    LEncoding := TIdUTF7Encoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GIdUTF7Encoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GIdUTF7Encoding;
end;

function IndyTextEncoding_UTF8: IIdTextEncoding;
{$IFNDEF DOTNET}
var
  LEncoding: IIdTextEncoding;
{$ENDIF}
begin
  if GIdUTF8Encoding = nil then begin
    {$IFDEF DOTNET}
    // TODO: use thread-safe assignment
    GIdUTF8Encoding := TIdDotNetEncoding.Create(System.Text.Encoding.UTF8);
    {$ELSE}
    LEncoding := TIdUTF8Encoding.Create;
    if InterlockedCompareExchangeIntf(IInterface(GIdUTF8Encoding), LEncoding, nil) <> nil then begin
      LEncoding := nil;
    end;
    {$ENDIF}
  end;
  Result := GIdUTF8Encoding;
end;

{$I IdDeprecatedImplBugOff.inc}
function enDefault: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := nil;
end;

{$I IdDeprecatedImplBugOff.inc}
function en7Bit: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IndyTextEncoding_ASCII;
end;

{$I IdDeprecatedImplBugOff.inc}
function en8Bit: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IndyTextEncoding_8Bit;
end;

{$I IdDeprecatedImplBugOff.inc}
function enUTF8: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IndyTextEncoding_UTF8;
end;

{$I IdDeprecatedImplBugOff.inc}
function Indy8BitEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    Result := TId8BitEncoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_8Bit;
end;

{$I IdDeprecatedImplBugOff.inc}
function IndyASCIIEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    Result := TIdASCIIEncoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_ASCII;
end;

{$I IdDeprecatedImplBugOff.inc}
function IndyUTF16BigEndianEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    Result := TIdUTF16BigEndianEncoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_UTF16BE;
end;

{$I IdDeprecatedImplBugOff.inc}
function IndyUTF16LittleEndianEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    Result := TIdUTF16LittleEndianEncoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_UTF16LE;
end;

{$I IdDeprecatedImplBugOff.inc}
function IndyOSDefaultEncoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    // TODO: SysUtils.TEncoding.Default uses ANSI on Windows
    // but uses UTF-8 on POSIX, so we should do the same...
    //Result := {$IFDEF WINDOWS}TIdMBCSEncoding{$ELSE}TIdUTF8Encoding{$ENDIF}.Create;
    Result := TIdMBCSEncoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_OSDefault;
end;

{$I IdDeprecatedImplBugOff.inc}
function IndyUTF7Encoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    Result := TIdUTF7Encoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_UTF7;
end;

{$I IdDeprecatedImplBugOff.inc}
function IndyUTF8Encoding{$IFNDEF DOTNET}(const AOwnedByIndy: Boolean = True){$ENDIF}: IIdTextEncoding;
{$I IdDeprecatedImplBugOn.inc}
begin
  {$IFNDEF DOTNET}
  if not AOwnedByIndy then begin
    Result := TIdUTF8Encoding.Create;
    Exit;
  end;
  {$ENDIF}
  Result := IndyTextEncoding_UTF8;
end;

{$IFDEF UNIX}
function HackLoadFileName(const ALibName, ALibVer : String) : string;  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF DARWIN}
  Result := ALibName+ALibVer+LIBEXT;
  {$ELSE}
    {$IFDEF IOS}
  Result := ALibName+ALibVer+LIBEXT;
    {$ELSE}
  Result := ALibName+LIBEXT+ALibVer;
    {$ENDIF}
  {$ENDIF}
end;

function HackLoad(const ALibName : String; const ALibVersions : array of String) : HMODULE;
var
  i : Integer;
begin
  Result := NilHandle;
  for i := Low(ALibVersions) to High(ALibVersions) do
  begin
    {$IFDEF USE_SAFELOADLIBRARY}
    Result := SafeLoadLibrary(HackLoadFileName(ALibName,ALibVersions[i]));
    {$ELSE}
      {$IFDEF KYLIXCOMPAT}
    // Workaround that is required under Linux (changed RTLD_GLOBAL with RTLD_LAZY Note: also work with LoadLibrary())
    Result := HMODULE(dlopen(PAnsiChar(HackLoadFileName(ALibName,ALibVersions[i])), RTLD_LAZY));
      {$ELSE}
    Result := LoadLibrary(HackLoadFileName(ALibName,ALibVersions[i]));
      {$ENDIF}
    {$ENDIF}
    {$IFDEF USE_INVALIDATE_MOD_CACHE}
    InvalidateModuleCache;
    {$ENDIF}
    if Result <> NilHandle then begin
      break;
    end;
  end;
end;
{$ENDIF}

procedure IndyRaiseLastError;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFNDEF HAS_RaiseLastOSError}
  RaiseLastWin32Error;
  {$ELSE}
  RaiseLastOSError;
  {$ENDIF}
end;

{$IFDEF HAS_Exception_RaiseOuterException}
procedure IndyRaiseOuterException(AOuterException: Exception);
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Exception.RaiseOuterException(AOuterException);
end;
{$ELSE}
  {$IFDEF DCC}
// RLebeau: There is no Exception.InnerException property to capture the inner
// exception into, but we can still raise the outer exception using Delphi's
// 'raise ... at [address]' syntax, at least.  This way, the debugger (and
// exception loggers) can show the outer exception occuring in the caller
// rather than inside this function...
    {$IFDEF HAS_System_ReturnAddress}
procedure IndyRaiseOuterException(AOuterException: Exception);
begin
  raise AOuterException at ReturnAddress;
end;
    {$ELSE}
// RLebeau: Delphi RTL functions like SysUtils.Abort(), Classes.TList.Error(),
// and Classes.TStrings.Error() raise their respective exceptions at the
// caller's return address using Delphi's 'raise ... at [address]' syntax,
// however they do so in different ways depending on Delphi version!
//
// ----------------
// SysUtils.Abort()
// ----------------
// Delphi 5-2007: Abort() calls an internal helper function that returns the
// caller's return address from the call stack - [EBP-4] in Delphi 5, [EBP+4]
// in Delphi 6+ - and then passes that value to 'raise'. Not sure why [EBP-4]
// was being used in Delphi 5.  Maybe a typo?
//
// Delphi 2009-XE: Abort() JMP's into an internal helper procedure that takes
// a Pointer parameter as input (passed in EAX) and passes it to 'raise'.
// Delphi 2009-2010 POP's the caller's return address from the call stack
// into EAX. Delphi XE simply MOV's [ESP] into EAX instead.
// ----------------
// TList.Error()
// TStrings.Error()
// ----------------
// Delphi 5-2010: Error() calls an internal helper function that returns the
// caller's return address from the call stack - always [EBP+4] - and then passes
// that value to 'raise'.
//
// Delphi XE: no helper is used. Error() is wrapped with {$O-} to force a stack
// frame, and then reads the caller's return address directly from the call stack
// (using pointer math to find it) and passes it to 'raise'.
// ----------------
//
// To be safe, we will use the MOV [ESP] approach here, as it is the simplest.
// We only have to worry about this in Delphi's Windows 32bit compiler, as the
// 64bit and mobile compilers have System.ReturnAddress available...

      // disable stack frames to reduce instructions
      {$I IdStackFramesOff.inc}
procedure IndyRaiseOuterException(AOuterException: Exception);
  procedure RaiseE(E: Exception; ReturnAddr: Pointer);
  begin
    raise E at ReturnAddr;
  end;
asm
  // AOuterException is already in EAX...
  // MOV EAX, AOuterException
  MOV EDX, [ESP]
  JMP RaiseE
end;
      {$I IdStackFramesOn.inc}

    {$ENDIF}
  {$ELSE}
// Not Delphi, so just raise the exception as-is until we know what else to do with it...
procedure IndyRaiseOuterException(AOuterException: Exception);
begin
  raise AOuterException;
end;
  {$ENDIF}
{$ENDIF}

{$IFNDEF DOTNET}
function InterlockedExchangeTHandle(var VTarget: THandle; const AValue: THandle): THandle;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_TInterlocked}
    {$IFDEF THANDLE_32}
  Result := THandle(TInterlocked.Exchange(Integer(VTarget), Integer(AValue)));
    {$ENDIF}
  //Temporary workaround.  TInterlocked for Emb really should accept 64 bit unsigned values as set of parameters
  //for TInterlocked.Exchange since 64-bit wide integers are common on 64 bit platforms.
    {$IFDEF THANDLE_64}
  Result := THandle(TInterlocked.Exchange(Int64(VTarget), Int64(AValue)));
    {$ENDIF}
  {$ELSE}
    {$IFDEF THANDLE_32}
  Result := THandle(InterlockedExchange(Integer(VTarget), Integer(AValue)));
    {$ENDIF}
    {$IFDEF THANDLE_64}
  Result := THandle(InterlockedExchange64(Int64(VTarget), Int64(AValue)));
    {$ENDIF}
  {$ENDIF}
end;

{$UNDEF DYNAMICLOAD_InterlockedCompareExchange}
{$IFNDEF HAS_TInterlocked}
  {$IFNDEF FPC}
    // RLebeau: InterlockedCompareExchange() is not available prior to Win2K,
    // so need to fallback to some other logic on older systems.  Not too many
    // people still support those systems anymore, so we will make this optional.
    //
    // InterlockedCompareExchange64(), on the other hand, is not available until
    // Windows Vista (and not defined in any version of Windows.pas up to Delphi
    // XE), so always dynamically load it in order to support WinXP 64-bit...

    {$IFDEF CPU64}
      {$DEFINE DYNAMICLOAD_InterlockedCompareExchange}
    {$ELSE}
      {.$DEFINE STATICLOAD_InterlockedCompareExchange}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF DYNAMICLOAD_InterlockedCompareExchange}
// See http://code.google.com/p/delphi-toolbox/source/browse/trunk/RTLEx/RTLEx.BasicOp.Atomic.pas
// for how to perform interlocked operations in assembler...

type
  TInterlockedCompareExchangeFunc = function(var Destination: PtrInt; Exchange, Comparand: PtrInt): PtrInt; stdcall;

var
  InterlockedCompareExchange: TInterlockedCompareExchangeFunc = nil;

function Impl_InterlockedCompareExchange(var Destination: PtrInt; Exchange, Comparand: PtrInt): PtrInt; stdcall;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF CPU64}
  // TODO: use LOCK CMPXCHG8B directly so this is more atomic...
  {$ELSE}
  // TODO: use LOCK CMPXCHG directly so this is more atomic...
  {$ENDIF}
  Result := Destination;
  if Destination = Comparand then begin
    Destination := Exchange;
  end;
end;

function Stub_InterlockedCompareExchange(var Destination: PtrInt; Exchange, Comparand: PtrInt): PtrInt; stdcall;

  function GetImpl: Pointer;
  const
    cKernel32 = 'KERNEL32'; {do not localize}
    // TODO: what is Embarcadero's 64-bit define going to be?
    cInterlockedCompareExchange = {$IFDEF CPU64}'InterlockedCompareExchange64'{$ELSE}'InterlockedCompareExchange'{$ENDIF}; {do not localize}
  begin
    Result := GetProcAddress(GetModuleHandle(cKernel32), cInterlockedCompareExchange);
    if Result = nil then begin
      Result := @Impl_InterlockedCompareExchange;
    end;
  end;

begin
  @InterlockedCompareExchange := GetImpl();
  Result := InterlockedCompareExchange(Destination, Exchange, Comparand);
end;
{$ENDIF}

function InterlockedCompareExchangePtr(var VTarget: Pointer; const AValue, Compare: Pointer): Pointer;
{$IFNDEF DYNAMICLOAD_InterlockedCompareExchange}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DYNAMICLOAD_InterlockedCompareExchange}
  Result := Pointer(IdGlobal.InterlockedCompareExchange(PtrInt(VTarget), PtrInt(AValue), PtrInt(Compare)));
  {$ELSE}
    {$IFDEF HAS_TInterlocked}
  Result := TInterlocked.CompareExchange(VTarget, AValue, Compare);
    {$ELSE}
      {$IFDEF HAS_InterlockedCompareExchangePointer}
  Result := InterlockedCompareExchangePointer(VTarget, AValue, Compare);
      {$ELSE}
        {$IFDEF HAS_InterlockedCompareExchange_Pointers}
        //work around a conflicting definition for InterlockedCompareExchange
  Result := {$IFDEF FPC}system.{$ENDIF}InterlockedCompareExchange(VTarget, AValue, Compare);
        {$ELSE}
          {$IFDEF FPC}
  Result := Pointer(
    {$IFDEF CPU64}InterlockedCompareExchange64{$ELSE}InterlockedCompareExchange{$ENDIF}
    (PtrInt(VTarget), PtrInt(AValue), PtrInt(Compare))
     );
          {$ELSE}
  // Delphi 64-bit is handled by HAS_InterlockedCompareExchangePointer
  Result := Pointer(InterlockedCompareExchange(Integer(VTarget), Integer(AValue), Integer(Compare)));
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function InterlockedCompareExchangeObj(var VTarget: TObject; const AValue, Compare: TObject): TObject;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF USE_OBJECT_ARC}
  // for ARC, we have to use the TObject overload of TInterlocked to ensure
  // that the reference counts of the objects are managed correctly...
  Result := TInterlocked.CompareExchange(VTarget, AValue, Compare);
  {$ELSE}
  Result := TObject(InterlockedCompareExchangePtr(Pointer(VTarget), Pointer(AValue), Pointer(Compare)));
  {$ENDIF}
end;

function InterlockedCompareExchangeIntf(var VTarget: IInterface; const AValue, Compare: IInterface): IInterface;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // we have to ensure that the reference counts of the interfaces are managed correctly...
  if AValue <> nil then begin
    AValue._AddRef;
  end;
  Result := IInterface(InterlockedCompareExchangePtr(Pointer(VTarget), Pointer(AValue), Pointer(Compare)));
  if (AValue <> nil) and (Pointer(Result) <> Pointer(Compare)) then begin
    AValue._Release;
  end;
end;
{$ENDIF}

{Little Endian Byte order functions from:

From: http://community.borland.com/article/0,1410,16854,00.html


Big-endian and little-endian formated integers - by Borland Developer Support Staff

Note that I will NOT do big Endian functions because the stacks can handle that
with HostToNetwork and NetworkToHost functions.

You should use these functions for writing data that sent and received in Little
Endian Form.  Do NOT assume endianness of what's written.  It can work in unpredictable
ways on other architectures.
}
function HostToLittleEndian(const AValue : UInt16) : UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  //I think that is Little Endian but I'm not completely sure
  Result := AValue;
  {$ELSE}
    {$IFDEF ENDIAN_LITTLE}
  Result := AValue;
    {$ENDIF}
    {$IFDEF ENDIAN_BIG}
  Result := swap(AValue);
    {$ENDIF}
  {$ENDIF}
end;

function HostToLittleEndian(const AValue : UInt32) : UInt32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  //I think that is Little Endian but I'm not completely sure
  Result := AValue;
  {$ELSE}
    {$IFDEF ENDIAN_LITTLE}
  Result := AValue;
    {$ENDIF}
    {$IFDEF ENDIAN_BIG}
  Result := swap(AValue shr 16) or (UInt32(swap(AValue and $FFFF)) shl 16);
    {$ENDIF}
  {$ENDIF}
end;

function HostToLittleEndian(const AValue : Integer) : Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  //I think that is Little Endian but I'm not completely sure
  Result := AValue;
  {$ELSE}
    {$IFDEF ENDIAN_LITTLE}
  Result := AValue;
    {$ENDIF}
    {$IFDEF ENDIAN_BIG}
  Result := swap(AValue);
    {$ENDIF}
  {$ENDIF}
end;

function LittleEndianToHost(const AValue : UInt16) : UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  //I think that is Little Endian but I'm not completely sure
  Result := AValue;
  {$ELSE}
    {$IFDEF ENDIAN_LITTLE}
  Result := AValue;
    {$ENDIF}
    {$IFDEF ENDIAN_BIG}
  Result := swap(AValue);
    {$ENDIF}
  {$ENDIF}
end;

function LittleEndianToHost(const AValue : UInt32): UInt32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  //I think that is Little Endian but I'm not completely sure
  Result := AValue;
  {$ELSE}
    {$IFDEF ENDIAN_LITTLE}
  Result := AValue;
    {$ENDIF}
    {$IFDEF ENDIAN_BIG}
  Result := swap(AValue shr 16) or (UInt32(swap(AValue and $FFFF)) shl 16);
    {$ENDIF}
  {$ENDIF}
end;

function LittleEndianToHost(const AValue : Integer): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  //I think that is Little Endian but I'm not completely sure
  Result := AValue;
  {$ELSE}
    {$IFDEF ENDIAN_LITTLE}
  Result := AValue;
    {$ENDIF}
    {$IFDEF ENDIAN_BIG}
  Result := Swap(AValue);
    {$ENDIF}
  {$ENDIF}
end;

// TODO: add an AIndex parameter
procedure FillBytes(var VBytes : TIdBytes; const ACount : Integer; const AValue : Byte);
{$IFDEF STRING_IS_ANSI}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
var
  I: Integer;
{$ENDIF}
begin
  // RLebeau: FillChar() is bad to use on Delphi/C++Builder 2009+ for filling
  // byte buffers as it is actually designed for filling character buffers
  // instead. Now that Char maps to WideChar, this causes problems for FillChar().
  {$IFDEF STRING_IS_UNICODE}
  //System.&Array.Clear(VBytes, 0, ACount);
  // TODO: optimize this
  for I := 0 to ACount-1 do begin
    VBytes[I] := AValue;
  end;
  {$ELSE}
  FillChar(VBytes[0], ACount, AValue);
  {$ENDIF}
end;

// RLebeau 10/22/2013: prior to Delphi 2010, fmCreate was an all-encompassing
// bitmask, no other flags could be combined with it.  The RTL was updated in
// Delphi 2010 to allow other flags to be specified along with fmCreate.  So
// at best, we will now be able to allow read-only access to other processes
// in Delphi 2010 and later, and at worst we will continue having exclusive
// right to the file in Delphi 2009 and earlier, just like we always did...

constructor TIdFileCreateStream.Create(const AFile : String);
begin
  inherited Create(AFile, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
end;

constructor TIdAppendFileStream.Create(const AFile : String);
var
  LFlags: Word;
begin
  LFlags := fmOpenReadWrite or fmShareDenyWrite;
  if not FileExists(AFile) then begin
    LFlags := LFLags or fmCreate;
  end;
  inherited Create(AFile, LFlags);
  if (LFlags and fmCreate) = 0 then begin
    TIdStreamHelper.Seek(Self, 0, soEnd);
  end;
end;

constructor TIdReadFileNonExclusiveStream.Create(const AFile : String);
begin
  inherited Create(AFile, fmOpenRead or fmShareDenyNone);
end;

constructor TIdReadFileExclusiveStream.Create(const AFile : String);
begin
  inherited Create(AFile, fmOpenRead or fmShareDenyWrite);
end;

function IsASCIILDH(const AByte: Byte): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := True;
  //Verify the absence of non-LDH ASCII code points; that is, the
  //absence of 0..2C, 2E..2F, 3A..40, 5B..60, and 7B..7F.
  //Permissable chars are in this set
  //['-','0'..'9','A'..'Z','a'..'z']
  if AByte <= $2C then begin
    Result := False;
  end
  else if (AByte >= $2E) and (AByte <= $2F) then begin
    Result := False;
  end
  else if (AByte >= $3A) and (AByte <= $40) then begin
    Result := False;
  end
  else if (AByte >= $5B) and (AByte <= $60) then begin
    Result := False;
  end
  else if (AByte >= $7B) and (AByte <= $7F) then begin
    Result := False;
  end;
end;

function IsASCIILDH(const ABytes: TIdBytes): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(ABytes)-1 do begin
    if not IsASCIILDH(ABytes[i]) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function IsASCII(const AByte: Byte): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AByte <= $7F;
end;

function IsASCII(const ABytes: TIdBytes): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(ABytes) -1 do begin
    if not IsASCII(ABytes[i]) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function StartsWithACE(const ABytes: TIdBytes): Boolean;
const
  cDash = Ord('-');
var
  LS: {$IFDEF STRING_IS_IMMUTABLE}TIdStringBuilder{$ELSE}string{$ENDIF};
begin
  Result := False;
  if Length(ABytes) >= 4 then
  begin
    if (ABytes[2] = cDash) and (ABytes[3] = cDash) then
    begin
      // TODO: just do byte comparisons so String conversions are not needed...
      {$IFDEF STRING_IS_IMMUTABLE}
      LS := TIdStringBuilder.Create(2);
      LS.Append(Char(ABytes[0]));
      LS.Append(Char(ABytes[1]));
      {$ELSE}
      SetLength(LS, 2);
      LS[1] := Char(ABytes[0]);
      LS[2] := Char(ABytes[1]);
      {$ENDIF}
      Result := PosInStrArray(LS{$IFDEF STRING_IS_IMMUTABLE}.ToString{$ENDIF},
        ['bl','bq','dq','lq','mq','ra','wq','zq'], False) > -1;{do not localize}
    end;
  end;
end;

function PosInSmallIntArray(const ASearchInt: Int16; const AArray: array of Int16): Integer;
begin
  for Result := Low(AArray) to High(AArray) do begin
    if ASearchInt = AArray[Result] then begin
      Exit;
    end;
  end;
  Result := -1;
end;

{This searches an array of string for an occurance of SearchStr}
function PosInStrArray(const SearchStr: string; const Contents: array of string; const CaseSensitive: Boolean = True): Integer;
begin
  for Result := Low(Contents) to High(Contents) do begin
    if CaseSensitive then begin
      if SearchStr = Contents[Result] then begin
        Exit;
      end;
    end else begin
      if TextIsSame(SearchStr, Contents[Result]) then begin
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

//IPv4 address conversion
function ByteToHex(const AByte: Byte): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF STRING_IS_IMMUTABLE}
var
  LSB: TIdStringBuilder;
{$ENDIF}
begin
  {$IFDEF STRING_IS_IMMUTABLE}
  LSB := TIdStringBuilder.Create(2);
  LSB.Append(IdHexDigits[(AByte and $F0) shr 4]);
  LSB.Append(IdHexDigits[AByte and $F]);
  Result := LSB.ToString;
  {$ELSE}
  SetLength(Result, 2);
  Result[1] := IdHexDigits[(AByte and $F0) shr 4];
  Result[2] := IdHexDigits[AByte and $F];
  {$ENDIF}
end;

function UInt32ToHex(const ALongWord : UInt32) : String;
 {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ByteToHex((ALongWord and $FF000000) shr 24)
            + ByteToHex((ALongWord and $00FF0000) shr 16)
            + ByteToHex((ALongWord and $0000FF00) shr 8)
            + ByteToHex(ALongWord and $000000FF);
end;

{$I IdDeprecatedImplBugOff.inc} 
function LongWordToHex(const ALongWord : UInt32) : String;
{$I IdDeprecatedImplBugOn.inc} 
 {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := UInt32ToHex(ALongWord);
end;

function ToHex(const AValue: TIdBytes; const ACount: Integer = -1;
  const AIndex: Integer = 0): string;
 {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  I, LCount: Integer;
  {$IFDEF STRING_IS_IMMUTABLE}
  LSB: TIdStringBuilder;
  {$ENDIF}
begin
  LCount := IndyLength(AValue, ACount, AIndex);
  if LCount > 0 then begin
    {$IFDEF STRING_IS_IMMUTABLE}
    LSB := TIdStringBuilder.Create(LCount*2);
    {$ELSE}
    SetLength(Result, LCount*2);
    {$ENDIF}
    for I := 0 to LCount-1 do begin
      {$IFDEF STRING_IS_IMMUTABLE}
      LSB.Append(IdHexDigits[(AValue[AIndex+I] and $F0) shr 4]);
      LSB.Append(IdHexDigits[AValue[AIndex+I] and $F]);
      {$ELSE}
      Result[I*2+1] := IdHexDigits[(AValue[AIndex+I] and $F0) shr 4];
      Result[I*2+2] := IdHexDigits[AValue[AIndex+I] and $F];
      {$ENDIF}
    end;
    {$IFDEF STRING_IS_IMMUTABLE}
    Result := LSB.ToString;
    {$ENDIF}
  end else begin
    Result := '';
  end;
end;

function ToHex(const AValue: array of UInt32): string;
var
  {$IFDEF STRING_IS_IMMUTABLE}
  LSB: TIdStringBuilder;
  {$ENDIF}
  P: {$IFDEF DOTNET}TIdBytes{$ELSE}PByteArray{$ENDIF};
  i, j: Integer;
begin
  Result := '';
  if Length(AValue) > 0 then
  begin
    {$IFDEF STRING_IS_IMMUTABLE}
    LSB := TIdStringBuilder.Create(Length(AValue)*SizeOf(UInt32)*2);
    {$ELSE}
    SetLength(Result, Length(AValue)*SizeOf(UInt32)*2);
    {$ENDIF}
    for i := 0 to High(AValue) do begin
      {$IFDEF DOTNET}
      P := ToBytes(AValue[i]);
      {$ELSE}
      P := PByteArray(@AValue[i]);
      {$ENDIF}
      for j := 0 to SizeOf(UInt32)-1 do begin
        {$IFDEF STRING_IS_IMMUTABLE}
        LSB.Append(IdHexDigits[(P[j] and $F0) shr 4]);
        LSB.Append(IdHexDigits[P[j] and $F]);
        {$ELSE}
        Result[(i*SizeOf(UInt32))+(j*2)+1] := IdHexDigits[(P^[j] and $F0) shr 4];
        Result[(i*SizeOf(UInt32))+(j*2)+2] := IdHexDigits[P^[j] and $F];
        {$ENDIF}
      end;
    end;//for
    {$IFDEF STRING_IS_IMMUTABLE}
    Result := LSB.ToString;
    {$ENDIF}
  end;
end;

function IPv4ToHex(const AIPAddress: string; const ADotted: Boolean): string;
var
  i: Integer;
  LBuf, LTmp: string;
begin
  LBuf := Trim(AIPAddress);
  Result := IdHexPrefix;
  for i := 0 to 3 do begin
    LTmp := ByteToHex(IndyStrToInt(Fetch(LBuf, '.', True)));
    if ADotted then begin
      Result := Result + '.' + IdHexPrefix + LTmp;
    end else begin
      Result := Result + LTmp;
    end;
  end;
end;

{$IFNDEF DOTNET}
function OctalToInt64(const AValue: string): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AValue) do begin
    Result := (Result shl 3) +  IndyStrToInt(AValue[i], 0);
  end;
end;
{$ENDIF}

function ByteToOctal(const AByte: Byte): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF STRING_IS_IMMUTABLE}
var
  LSB: TIdStringBuilder;
  C: Char;
{$ENDIF}
begin
  {$IFDEF STRING_IS_IMMUTABLE}
  C := IdOctalDigits[(AByte shr 6) and $7];
  if C <> '0' then begin
    LSB := TIdStringBuilder.Create(4);
    LSB.Append(Char('0')); {do not localize}
  end else begin
    LSB := TIdStringBuilder.Create(3);
  end;
  LSB.Append(C);
  LSB.Append(IdOctalDigits[(AByte shr 3) and $7]);
  LSB.Append(IdOctalDigits[AByte and $7]);
  Result := LSB.ToString;
  {$ELSE}
  SetLength(Result, 3);
  Result[1] := IdOctalDigits[(AByte shr 6) and $7];
  Result[2] := IdOctalDigits[(AByte shr 3) and $7];
  Result[3] := IdOctalDigits[AByte and $7];
  if Result[1] <> '0' then begin {do not localize}
    Result := '0' + Result; {do not localize}
  end;
  {$ENDIF}
end;

function IPv4ToOctal(const AIPAddress: string): string;
var
  i: Integer;
  LBuf: string;
begin
  LBuf := Trim(AIPAddress);
  Result := ByteToOctal(IndyStrToInt(Fetch(LBuf, '.', True), 0));
  for i := 0 to 2 do begin
    Result := Result + '.' + ByteToOctal(IndyStrToInt(Fetch(LBuf, '.', True), 0));
  end;
end;

procedure CopyTIdBytes(const ASource: TIdBytes; const ASourceIndex: Integer;
  var VDest: TIdBytes; const ADestIndex: Integer; const ALength: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  System.array.Copy(ASource, ASourceIndex, VDest, ADestIndex, ALength);
  {$ELSE}
  //if these asserts fail, then it indicates an attempted buffer overrun.
  Assert(ASourceIndex >= 0);
  Assert((ASourceIndex+ALength) <= Length(ASource));
  Move(ASource[ASourceIndex], VDest[ADestIndex], ALength);
  {$ENDIF}
end;

procedure CopyTIdChar(const ASource: Char; var VDest: TIdBytes; const ADestIndex: Integer;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  LChars: {$IFDEF DOTNET}array[0..0] of Char{$ELSE}TIdWideChars{$ENDIF};
begin
  EnsureEncoding(ADestEncoding);
  {$IFDEF STRING_IS_UNICODE}
    {$IFNDEF DOTNET}
  SetLength(LChars, 1);
    {$ENDIF}
  LChars[0] := ASource;
  ADestEncoding.GetBytes(LChars, 0, 1, VDest, ADestIndex);
  {$ELSE}
  EnsureEncoding(ASrcEncoding, encOSDefault);
  LChars := ASrcEncoding.GetChars(RawToBytes(ASource, 1));
  ADestEncoding.GetBytes(LChars, 0, Length(LChars), VDest, ADestIndex);
  {$ENDIF}
end;

procedure CopyTIdInt16(const ASource: Int16; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  LShort : TIdBytes;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  LShort := System.BitConverter.GetBytes(ASource);
  System.array.Copy(LShort, 0, VDest, ADestIndex, SizeOf(Int16));
  {$ELSE}
  PInt16(@VDest[ADestIndex])^ := ASource;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
procedure CopyTIdShort(const ASource: Int16; var VDest: TIdBytes; const ADestIndex: Integer);
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  CopyTIdInt16(ASource, VDest, ADestIndex);
end;

procedure CopyTIdUInt16(const ASource: UInt16; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  LWord : TIdBytes;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  LWord := System.BitConverter.GetBytes(ASource);
  System.array.Copy(LWord, 0, VDest, ADestIndex, SizeOf(UInt16));
  {$ELSE}
  PUInt16(@VDest[ADestIndex])^ := ASource;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
procedure CopyTIdWord(const ASource: UInt16; var VDest: TIdBytes; const ADestIndex: Integer);
{$I IdDeprecatedImplBugOn.inc}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  CopyTIdUInt16(ASource, VDest, ADestIndex);
end;

procedure CopyTIdUInt32(const ASource: UInt32; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  LWord : TIdBytes;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  LWord := System.BitConverter.GetBytes(ASource);
  System.array.Copy(LWord, 0, VDest, ADestIndex, SizeOf(UInt32));
  {$ELSE}
  PUInt32(@VDest[ADestIndex])^ := ASource;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
procedure CopyTIdLongWord(const ASource: UInt32; var VDest: TIdBytes; const ADestIndex: Integer);
{$I IdDeprecatedImplBugOn.inc}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  CopyTIdUInt32(ASource, VDest, ADestIndex);
end;

procedure CopyTIdInt32(const ASource: Int32; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  LInt : TIdBytes;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  LInt := System.BitConverter.GetBytes(ASource);
  System.array.Copy(LInt, 0, VDest, ADestIndex, SizeOf(Int32));
  {$ELSE}
  PInt32(@VDest[ADestIndex])^ := ASource;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
procedure CopyTIdLongInt(const ASource: Int32; var VDest: TIdBytes; const ADestIndex: Integer);
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  CopyTIdInt32(ASource, VDest, ADestIndex);
end;

procedure CopyTIdInt64(const ASource: Int64; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  LWord : TIdBytes;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  LWord := System.BitConverter.GetBytes(ASource);
  System.array.Copy(LWord, 0, VDest, ADestIndex, SizeOf(Int64));
  {$ELSE}
  PInt64(@VDest[ADestIndex])^ := ASource;
  {$ENDIF}
end;

procedure CopyTIdUInt64(const ASource: TIdUInt64;
  var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  LWord : TIdBytes;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  LWord := System.BitConverter.GetBytes(ASource);
  System.array.Copy(LWord, 0, VDest, ADestIndex, SizeOf(UInt64));
  {$ELSE}
  PUInt64(@VDest[ADestIndex])^ := ASource{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
  {$ENDIF}
end;

{$IFDEF UInt64_IS_NATIVE}
  {$IFDEF TIdUInt64_HAS_QuadPart}
    {$DEFINE USE_TIdTicks_TIdUInt64_CONVERSION}
  {$ENDIF}
{$ENDIF}

procedure CopyTIdTicks(const ASource: TIdTicks; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF USE_TIdTicks_TIdUInt64_CONVERSION}
var
  LValue: TIdUInt64;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF USE_TIdTicks_TIdUInt64_CONVERSION}
  // In C++Builder 2006/2007, TIdUInt64 is a packed record, but TIdTicks is
  // an alias for a native UInt64, so need a conversion here to get around
  // a compiler error: "E2010 Incompatible types: 'TIdUInt64' and 'UInt64'"...
  LValue.QuadPart := ASource;
  CopyTIdUInt64(LValue, VDest, ADestIndex);
  {$ELSE}
    {$IFDEF UInt64_IS_NATIVE}
  CopyTIdUInt64(ASource, VDest, ADestIndex);
    {$ELSE}
  CopyTIdInt64(ASource, VDest, ADestIndex);
    {$ENDIF}
  {$ENDIF}
end;

procedure CopyTIdIPV6Address(const ASource: TIdIPv6Address; var VDest: TIdBytes; const ADestIndex: Integer);
{$IFDEF DOTNET}
var
  i : Integer;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  for i := 0 to 7 do begin
    CopyTIdUInt16(ASource[i], VDest, ADestIndex + (i * 2));
  end;
  {$ELSE}
  Move(ASource, VDest[ADestIndex], 16);
  {$ENDIF}
end;

procedure CopyTIdByteArray(const ASource: array of Byte; const ASourceIndex: Integer;
  var VDest: TIdBytes; const ADestIndex: Integer; const ALength: Integer);
begin
  {$IFDEF DOTNET}
  System.array.Copy(ASource, ASourceIndex, VDest, ADestIndex, ALength);
  {$ELSE}
  Move(ASource[ASourceIndex], VDest[ADestIndex], ALength);
  {$ENDIF}
end;

procedure CopyTIdString(const ASource: String; var VDest: TIdBytes;
  const ADestIndex: Integer; const ALength: Integer = -1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  CopyTIdString(ASource, 1, VDest, ADestIndex, ALength, ADestEncoding
    {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
    );
end;

procedure CopyTIdString(const ASource: String; const ASourceIndex: Integer;
  var VDest: TIdBytes; const ADestIndex: Integer; const ALength: Integer = -1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ); overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LLength: Integer;
  {$IFDEF STRING_IS_ANSI}
  LTmp: TIdWideChars;
  {$ENDIF}
begin
  {$IFDEF STRING_IS_ANSI}
  LTmp := nil; // keep the compiler happy
  {$ENDIF}
  LLength := IndyLength(ASource, ALength, ASourceIndex);
  if LLength > 0 then begin
    EnsureEncoding(ADestEncoding);
    {$IFDEF STRING_IS_UNICODE}
    ADestEncoding.GetBytes(ASource, ASourceIndex, LLength, VDest, ADestIndex);
    {$ELSE}
    EnsureEncoding(ASrcEncoding, encOSDefault);
    LTmp := ASrcEncoding.GetChars(RawToBytes(ASource[ASourceIndex], LLength)); // convert to Unicode
    ADestEncoding.GetBytes(LTmp, 0, Length(LTmp), VDest, ADestIndex);
    {$ENDIF}
  end;
end;

// TODO: define STRING_UNICODE_MISMATCH for WinCE in IdCompilerDefines.inc?
{$IFDEF WINDOWS}
  {$IFDEF WINCE}
    {$IFNDEF STRING_IS_UNICODE}
      {$DEFINE DEBUG_STRING_MISMATCH}
    {$ENDIF}
  {$ELSE}
    {$IFDEF STRING_UNICODE_MISMATCH}
      {$DEFINE DEBUG_STRING_MISMATCH}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

procedure DebugOutput(const AText: string);
{$IFDEF DEBUG_STRING_MISMATCH}
var
  LTemp: {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF};
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF KYLIX}
  __write(stderr, AText, Length(AText));
  __write(stderr, EOL, Length(EOL));
  {$ENDIF}

  {$IFDEF WINDOWS}
    {$IFDEF DEBUG_STRING_MISMATCH}
  LTemp := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(AText); // explicit convert to Ansi/Unicode
  OutputDebugString({$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LTemp));
    {$ELSE}
  OutputDebugString(PChar(AText));
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DOTNET}
  System.Diagnostics.Debug.WriteLine(AText);
  {$ENDIF}
end;

function CurrentThreadId: TIdThreadID;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
{$IFDEF DOTNET}
  {$IFDEF DOTNET_2_OR_ABOVE}
  {
  [Warning] IdGlobal.pas(1416): W1000 Symbol 'GetCurrentThreadId'
  is deprecated: 'AppDomain.GetCurrentThreadId has been deprecated because
  it does not provide a stable Id when managed threads are running on fibers
  (aka lightweight threads). To get a stable identifier for a managed thread,
  use the ManagedThreadId property on Thread.
  http://go.microsoft.com/fwlink/?linkid=14202'
  }
  Result := System.Threading.Thread.CurrentThread.ManagedThreadId;
   // Thread.ManagedThreadId;
  {$ENDIF}
  {$IFDEF DOTNET_1_1}
  // SG: I'm not sure if this return the handle of the dotnet thread or the handle of the application domain itself (or even if there is a difference)
  Result := AppDomain.GetCurrentThreadId;
  // RLebeau
  // TODO: find if there is something like the following instead:
  // System.Diagnostics.Thread.GetCurrentThread.ID
  // System.Threading.Thread.CurrentThread.ID
  {$ENDIF}
{$ELSE}
  // TODO: is GetCurrentThreadId() available on Linux?
  Result := GetCurrentThreadID;
{$ENDIF}
end;

{$UNDEF KYLIXCOMPAT_OR_VCL_POSIX}
{$IFDEF KYLIXCOMPAT}
  {$DEFINE KYLIXCOMPAT_OR_VCL_POSIX}
{$ENDIF}
{$IFDEF USE_VCL_POSIX}
  {$DEFINE KYLIXCOMPAT_OR_VCL_POSIX}
{$ENDIF}

function CurrentProcessId: TIdPID;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.Diagnostics.Process.GetCurrentProcess.ID;
  {$ELSE}
    {$IFDEF WINDOWS}
  Result := GetCurrentProcessID;
    {$ELSE}
      {$IFDEF KYLIXCOMPAT_OR_VCL_POSIX}
  Result := getpid;
      {$ELSE}
        {$IFDEF USE_BASEUNIX}
  Result := fpgetpid;
        {$ELSE}
  {$message error CurrentProcessId is not implemented on this platform!}
  Result := 0;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function Fetch(var AInput: string; const ADelim: string = IdFetchDelimDefault;
  const ADelete: Boolean = IdFetchDeleteDefault;
  const ACaseSensitive: Boolean = IdFetchCaseSensitiveDefault): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LPos: Integer;
begin
  if ACaseSensitive then begin
    if ADelim = #0 then begin
      // AnsiPos does not work with #0
      LPos := Pos(ADelim, AInput);
    end else begin
      LPos := IndyPos(ADelim, AInput);
    end;
    if LPos = 0 then begin
      Result := AInput;
      if ADelete then begin
        AInput := '';    {Do not Localize}
      end;
    end
    else begin
      Result := Copy(AInput, 1, LPos - 1);
      if ADelete then begin
        //slower Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
        //remaining part is larger than the deleted
        AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
      end;
    end;
  end else begin
    Result := FetchCaseInsensitive(AInput, ADelim, ADelete);
  end;
end;

function FetchCaseInsensitive(var AInput: string; const ADelim: string;
  const ADelete: Boolean): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LPos: Integer;
begin
  if ADelim = #0 then begin
    // AnsiPos does not work with #0
    LPos := Pos(ADelim, AInput);
  end else begin
    //? may be AnsiUpperCase?
    LPos := IndyPos(UpperCase(ADelim), UpperCase(AInput));
  end;
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then begin
      AInput := '';    {Do not Localize}
    end;
  end else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then begin
      //faster than Delete(AInput, 1, LPos + Length(ADelim) - 1); because the
      //remaining part is larger than the deleted
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
    end;
  end;
end;

function GetThreadHandle(AThread: TThread): TIdThreadHandle;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := AThread.ThreadID; // RLebeau: is it right to return an ID where a thread object handle is expected instead?
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := AThread.Handle;
  {$ENDIF}
  {$IFDEF DOTNET}
  Result := AThread.Handle;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
function Ticks: UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // TODO: maybe throw an exception if Ticks64() exceeds the 49.7 day limit of UInt32?
  Result := UInt32(Ticks64() mod High(UInt32));
end;

// RLebeau: breaking up the Ticks64() implementation into separate platform blocks,
// instead of trying to do it all in one implementation.  This way, the code is
// cleaner, and if I miss a platform then the compiler should complain about Ticks64()
// being unresolved...

// TODO: move these to platform-specific units instead, maybe even to the TIdStack classes?

{$IFDEF DOTNET}
function Ticks64: TIdTicks;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // Must cast to a cardinal
  //
  // http://lists.ximian.com/archives/public/mono-bugs/2003-November/009293.html
  // Other references in Google.
  // Bug in .NET. It acts like Win32, not as per .NET docs but goes negative after 25 days.
  //
  // There may be a problem in the future if .NET changes this to work as docced with 25 days.
  // Will need to check our routines then and somehow counteract / detect this.
  // One possibility is that we could just wrap it ourselves in this routine.

  // TODO: use DateTime.Ticks instead?
  //Result := DateTime.Now.Ticks div 10000;

  Result := TIdTicks(Environment.TickCount);
end;

{$ELSE}
  {$IFDEF WINDOWS}

type
  TGetTickCount64Func = function: UInt64; stdcall;

var
  GetTickCount64: TGetTickCount64Func = nil;

function Impl_GetTickCount64: UInt64; stdcall;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // TODO: implement some kind of accumulator so the Result
  // keeps growing even when GetTickCount() wraps back to 0
  Result := UInt64(Windows.GetTickCount);
end;

function Stub_GetTickCount64: UInt64; stdcall;

  function GetImpl: Pointer;
  begin
    Result := GetProcAddress(GetModuleHandle('KERNEL32'), 'GetTickCount64'); {do not localize}
    if Result = nil then begin
      Result := @Impl_GetTickCount64;
    end;
  end;

begin
  @GetTickCount64 := GetImpl();
  Result := GetTickCount64();
end;

function Ticks64: TIdTicks;
  {$IFDEF USE_HI_PERF_COUNTER_FOR_TICKS}
var
  nTime, freq: {$IFDEF WINCE}LARGE_INTEGER{$ELSE}Int64{$ENDIF};
{$ENDIF}
begin
  // S.G. 27/11/2002: Changed to use high-performance counters as per suggested
  // S.G. 27/11/2002: by David B. Ferguson (david.mcs@ns.sympatico.ca)

  // RLebeau 11/12/2009: removed the high-performance counters again.  They
  // are not reliable on multi-core systems, and are now starting to cause
  // problems with TIdIOHandler.ReadLn() timeouts under Windows XP SP3, both
  // 32-bit and 64-bit.  Refer to these discussions:
  //
  // http://www.virtualdub.org/blog/pivot/entry.php?id=106
  // http://blogs.msdn.com/oldnewthing/archive/2008/09/08/8931563.aspx

  {$IFDEF USE_HI_PERF_COUNTER_FOR_TICKS}
    {$IFDEF WINCE}
  if Windows.QueryPerformanceCounter(@nTime) then begin
    if Windows.QueryPerformanceFrequency(@freq) then begin
      Result := Trunc((nTime.QuadPart / Freq.QuadPart) * 1000) and High(TIdTicks);
      Exit;
    end;
  end;
    {$ELSE}
  if Windows.QueryPerformanceCounter(nTime) then begin
    if Windows.QueryPerformanceFrequency(freq) then begin
      Result := Trunc((nTime / Freq) * 1000) and High(TIdTicks);
      Exit;
    end;
  end;
    {$ENDIF}
  {$ENDIF}

  Result := TIdTicks(GetTickCount64());
end;

  {$ELSE}
    {$IFDEF USE_clock_gettime}

  {$IFDEF LINUX}
// according to Linux's /usr/include/linux/time.h
const
  CLOCK_MONOTONIC = 1;
  {$ENDIF}
  {$IFDEF FREEBSD}
// according to FreeBSD's /usr/include/time.h
const
  CLOCK_MONOTONIC = 4;
  {$ENDIF}
  {$IFDEF ANDROID}
// according to Android NDK's /include/time.h
const
  CLOCK_MONOTONIC = 1;
  {$ENDIF}

function clock_gettime(clockid: Integer; var pts: timespec): Integer; cdecl; external 'libc';

function Ticks64: TIdTicks;
var
  ts: timespec;
begin
  // TODO: use CLOCK_BOOTTIME on platforms that support it?  It takes system
  // suspension into account, whereas CLOCK_MONOTONIC does not...
  clock_gettime(CLOCK_MONOTONIC, ts);

  {$I IdRangeCheckingOff.inc}
  {$I IdOverflowCheckingOff.inc}
  Result := (Int64(ts.tv_sec) * 1000) + (ts.tv_nsec div 1000000);
  {$I IdOverflowCheckingOn.inc}
  {$I IdRangeCheckingOn.inc}
end;

    {$ELSE}
      {$IFDEF UNIX}

  {$IFDEF DARWIN}
    {$IFDEF FPC}
//RLebeau: FPC does not provide mach_timebase_info() and mach_absolute_time() yet...
function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): Integer; cdecl; external 'libc';
function mach_absolute_time: QWORD; cdecl; external 'libc';
    {$ENDIF}
  {$ENDIF}

function Ticks64: TIdTicks;
  {$IFDEF DARWIN}
    {$IFDEF USE_INLINE} inline;{$ENDIF}
  {$ELSE}
var
  tv: timeval;
  {$ENDIF}
begin
  {$IFDEF DARWIN}

  // TODO: mach_absolute_time() does NOT count ticks while the system is
  // sleeping! We can use time() to account for that:
  //
  // "time() carries on incrementing while the device is asleep, but of
  // course can be manipulated by the operating system or user. However,
  // the Kernel boottime (a timestamp of when the system last booted)
  // also changes when the system clock is changed, therefore even though
  // both these values are not fixed, the offset between them is."
  //
  // time_t uptime()
  // {
  //   struct timeval boottime;
  //   int mib[2] = {CTL_KERN, KERN_BOOTTIME};
  //   size_t size = sizeof(boottime);
  //   time_t now;
  //   time_t uptime = -1;
  //   time(&now);
  //   if ((sysctl(mib, 2, &boottime, &size, NULL, 0) != -1) && (boottime.tv_sec != 0))
  //   {
  //     uptime = now - boottime.tv_sec;
  //   }
  //   return uptime;
  // }
  //
  // However, KERN_BOOTTIME only has *seconds* precision (timeval.tv_usecs is always 0).

  // mach_absolute_time() returns billionth of seconds, so divide by one million to get milliseconds
  Result := (mach_absolute_time() * GMachTimeBaseInfo.numer) div (1000000 * GMachTimeBaseInfo.denom);

  {$ELSE}

    {$IFDEF KYLIXCOMPAT_OR_VCL_POSIX}
  gettimeofday(tv, nil);
    {$ELSE}
      {$IFDEF USE_BASEUNIX}
  fpgettimeofday(@tv,nil);
      {$ELSE}
  {$message error gettimeofday is not called on this platform!}
  FillChar(tv, sizeof(tv), 0);
      {$ENDIF}
    {$ENDIF}

  {
  I've implemented this correctly for now. I'll argue for using
  an int64 internally, since apparently quite some functionality
  (throttle, etc etc) depends on it, and this value may wrap
  at any point in time.
  For Windows: Uptime > 72 hours isn't really that rare any more,
  For Linux: no control over when this wraps.

  IdEcho has code to circumvent the wrap, but its not very good
  to have code for that at all spots where it might be relevant.
  }

  {$I IdRangeCheckingOff.inc}
  Result := (Int64(tv.tv_sec) * 1000) + (tv.tv_usec div 1000);
  {$I IdRangeCheckingOn.inc}

  {$ENDIF}

end;

      {$ELSE}

function Ticks64: TIdTicks;
begin
  {$message error Ticks64 is not implemented on this platform!}
  Result := 0;
end;

      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$I IdDeprecatedImplBugOff.inc}
function GetTickDiff(const AOldTickCount, ANewTickCount: UInt32): UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {This is just in case the TickCount rolled back to zero}
  if ANewTickCount >= AOldTickCount then begin
    Result := ANewTickCount - AOldTickCount;
  end else begin
    Result := ((High(UInt32) - AOldTickCount) + ANewTickCount) + 1;
  end;
end;

function GetTickDiff64(const AOldTickCount, ANewTickCount: TIdTicks): TIdTicks;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {This is just in case the TickCount rolled back to zero}
  if ANewTickCount >= AOldTickCount then begin
    Result := TIdTicks(ANewTickCount - AOldTickCount);
  end else begin
    Result := TIdTicks(((High(TIdTicks) - AOldTickCount) + ANewTickCount) + 1);
  end;
end;

function GetElapsedTicks(const AOldTickCount: TIdTicks): UInt32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := UInt32(GetTickDiff64(AOldTickCount, Ticks64));
end;

function GetElapsedTicks64(const AOldTickCount: TIdTicks): TIdTicks;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := GetTickDiff64(AOldTickCount, Ticks64);
end;

{$IFNDEF DOTNET}
function ServicesFilePath: string;
var
  {$IFDEF WINDOWS}
  sLocation: {$IFDEF STRING_UNICODE_MISMATCH}TIdPlatformString{$ELSE}string{$ENDIF};
  {$ELSE}
  sLocation: string;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  sLocation := '/etc/';  // assume Berkeley standard placement   {do not localize}
  {$ENDIF}

  {$IFDEF WINDOWS}
    {$IFNDEF WINCE}
  SetLength(sLocation, MAX_PATH);
  SetLength(sLocation, GetWindowsDirectory(
    {$IFDEF STRING_UNICODE_MISMATCH}PIdPlatformChar(sLocation){$ELSE}PChar(sLocation){$ENDIF}
    , MAX_PATH));
  sLocation := IndyIncludeTrailingPathDelimiter(sLocation);
  if IndyWindowsPlatform = VER_PLATFORM_WIN32_NT then begin
    sLocation := sLocation + 'system32\drivers\etc\'; {do not localize}
  end;
    {$ELSE}
  // GetWindowsDirectory() does not exist in WinCE, and there is no system folder, either
  sLocation := '\Windows\'; {do not localize}
    {$ENDIF}
  {$ENDIF}

  Result := sLocation + 'services'; {do not localize}
end;
{$ENDIF}


{$IFNDEF DOTNET}
// IdPorts returns a list of defined ports in /etc/services
function IdPorts: TIdPortList;
var
  s: string;
  idx, iPosSlash: {$IFDEF BYTE_COMPARE_SETS}Byte{$ELSE}Integer{$ENDIF};
  i: {$IFDEF HAS_GENERICS_TList}Integer{$ELSE}PtrInt{$ENDIF};
  iPrev: PtrInt;
  sl: TStringList;
begin
  if GIdPorts = nil then
  begin
    GIdPorts := TIdPortList.Create;
    sl := TStringList.Create;
    try
      // TODO: use TStreamReader instead, on versions that support it
      sl.LoadFromFile(ServicesFilePath);  {do not localize}
      iPrev := 0;
      for idx := 0 to sl.Count - 1 do
      begin
        s := sl[idx];
        iPosSlash := IndyPos('/', s);   {do not localize}
        if (iPosSlash > 0) and (not (IndyPos('#', s) in [1..iPosSlash])) then {do not localize}
        begin // presumably found a port number that isn't commented    {Do not Localize}
          i := iPosSlash;
          repeat
            Dec(i);
            if i = 0 then begin
              raise EIdCorruptServicesFile.CreateFmt(RSCorruptServicesFile, [ServicesFilePath]); {do not localize}
            end;
          //TODO: Make Whitespace a function to elim warning
          until Ord(s[i]) in IdWhiteSpace;
          i := IndyStrToInt(Copy(s, i+1, iPosSlash-i-1));
          if i <> iPrev then begin
            GIdPorts.Add(
              {$IFDEF HAS_GENERICS_TList}i{$ELSE}Pointer(i){$ENDIF}
            );
          end;
          iPrev := i;
        end;
      end;
    finally
      sl.Free;
    end;
  end;
  Result := GIdPorts;
end;
{$ENDIF}

function iif(ATest: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ATest then begin
    Result := ATrue;
  end else begin
    Result := AFalse;
  end;
end;

function iif(ATest: Boolean; const ATrue: string; const AFalse: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ATest then begin
    Result := ATrue;
  end else begin
    Result := AFalse;
  end;
end;

function iif(ATest: Boolean; const ATrue: Boolean; const AFalse: Boolean): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ATest then begin
    Result := ATrue;
  end else begin
    Result := AFalse;
  end;
end;

function iif(const AEncoding, ADefEncoding: IIdTextEncoding; ADefEncodingType: IdTextEncodingType = encASCII): IIdTextEncoding;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AEncoding;
  if Result = nil then
  begin
    Result := ADefEncoding;
    EnsureEncoding(Result, ADefEncodingType);
  end;
end;

function InMainThread: Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.Threading.Thread.CurrentThread = MainThread;
  {$ELSE}
  Result := GetCurrentThreadID = MainThreadID;
  {$ENDIF}
end;

procedure WriteMemoryStreamToStream(Src: TMemoryStream; Dest: TStream; Count: TIdStreamSize);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Dest.Write(Src.Memory, Count);
  {$ELSE}
  Dest.Write(Src.Memory^, Count);
  {$ENDIF}
end;

{$IFNDEF DOTNET_EXCLUDE}
function IsCurrentThread(AThread: TThread): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := AThread.ThreadID = GetCurrentThreadID;
end;
{$ENDIF}

//convert a dword into an IPv4 address in dotted form
function MakeUInt32IntoIPv4Address(const ADWord: UInt32): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IntToStr((ADWord shr 24) and $FF) + '.';
  Result := Result + IntToStr((ADWord shr 16) and $FF) + '.';
  Result := Result + IntToStr((ADWord shr 8) and $FF) + '.';
  Result := Result + IntToStr(ADWord and $FF);
end;

{$I IdDeprecatedImplBugOff.inc}
function MakeDWordIntoIPv4Address(const ADWord: UInt32): string;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := MakeUInt32IntoIPv4Address(ADWord);
end;

function IsAlpha(const AChar: Char): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // TODO: under XE3.5+, use TCharHelper.IsLetter() instead
  // TODO: under D2009+, use TCharacter.IsLetter() instead

  // Do not use IsCharAlpha or IsCharAlphaNumeric - they are Win32 routines
  Result := ((AChar >= 'a') and (AChar <= 'z')) or ((AChar >= 'A') and (AChar <= 'Z')); {Do not Localize}
end;

function IsAlpha(const AString: String; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  i: Integer;
  LLen: Integer;
begin
  Result := False;
  LLen := IndyLength(AString, ALength, AIndex);
  if LLen > 0 then begin
    for i := 0 to LLen-1 do begin
      if not IsAlpha(AString[AIndex+i]) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function IsAlphaNumeric(const AChar: Char): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // Do not use IsCharAlpha or IsCharAlphaNumeric - they are Win32 routines
  Result := IsAlpha(AChar) or IsNumeric(AChar);
end;

function IsAlphaNumeric(const AString: String; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  i: Integer;
  LLen: Integer;
begin
  Result := False;
  LLen := IndyLength(AString, ALength, AIndex);
  if LLen > 0 then begin
    for i := 0 to LLen-1 do begin
      if not IsAlphaNumeric(AString[AIndex+i]) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function IsOctal(const AChar: Char): Boolean; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := (AChar >= '0') and (AChar <= '7') {Do not Localize}
end;

function IsOctal(const AString: string; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  i: Integer;
  LLen: Integer;
begin
  Result := False;
  LLen := IndyLength(AString, ALength, AIndex);
  if LLen > 0 then begin
    for i := 0 to LLen-1 do begin
      if not IsOctal(AString[AIndex+i]) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function IsHexidecimal(const AChar: Char): Boolean; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IsNumeric(AChar)
   or ((AChar >= 'A') and (AChar <= 'F')) {Do not Localize}
   or ((AChar >= 'a') and (AChar <= 'f')); {Do not Localize}
end;

function IsHexidecimal(const AString: string; const ALength: Integer = -1; const AIndex: Integer = 1): Boolean; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  i: Integer;
  LLen: Integer;
begin
  Result := False;
  LLen := IndyLength(AString, ALength, AIndex);
  if LLen > 0 then begin
    for i := 0 to LLen-1 do begin
      if not IsHexidecimal(AString[AIndex+i]) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

{$HINTS OFF}
function IsNumeric(const AString: string): Boolean;
var
  LCode: Integer;
  LVoid: Int64;
begin
  Val(AString, LVoid, LCode);
  Result := LCode = 0;
end;
{$HINTS ON}

function IsNumeric(const AString: string; const ALength: Integer; const AIndex: Integer = 1): Boolean;
var
  I: Integer;
  LLen: Integer;
begin
  Result := False;
  LLen := IndyLength(AString, ALength, AIndex);
  if LLen > 0 then begin
    for I := 0 to LLen-1 do begin
      if not IsNumeric(AString[AIndex+i]) then begin
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function IsNumeric(const AChar: Char): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // TODO: under XE3.5+, use TCharHelper.IsDigit() instead
  // TODO: under D2009+, use TCharacter.IsDigit() instead

  // Do not use IsCharAlpha or IsCharAlphaNumeric - they are Win32 routines
  Result := (AChar >= '0') and (AChar <= '9'); {Do not Localize}
end;

{
This is an adaptation of the StrToInt64 routine in SysUtils.
We had to adapt it to work with Int64 because the one with Integers
can not deal with anything greater than MaxInt and IP addresses are
always $0-$FFFFFFFF (unsigned)
}
{$IFNDEF HAS_StrToInt64Def}
function StrToInt64Def(const S: string; const Default: Integer): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then begin
    Result := Default;
  end;
end;
{$ENDIF}

function IPv4MakeUInt32InRange(const AInt: Int64; const A256Power: Integer): UInt32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
//Note that this function is only for stripping off some extra bits
//from an address that might appear in some spam E-Mails.
begin
  case A256Power of
    4: Result := (AInt and POWER_4);
    3: Result := (AInt and POWER_3);
    2: Result := (AInt and POWER_2);
  else
    Result := (AInt and POWER_1);
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function IPv4MakeLongWordInRange(const AInt: Int64; const A256Power: Integer): UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IPv4MakeUInt32InRange(AInt, A256Power);
end;

function IPv4ToUInt32(const AIPAddress: string): UInt32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LErr: Boolean;
begin
  Result := IPv4ToUInt32(AIPAddress, LErr);
end;

{$I IdDeprecatedImplBugOff.inc}
function IPv4ToDWord(const AIPAddress: string): UInt32; overload;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IPv4ToUInt32(AIPAddress);
end;

function IPv4ToUInt32(const AIPAddress: string; var VErr: Boolean): UInt32;
var
{$IFDEF DOTNET}
  AIPaddr: IPAddress;
{$ELSE}
  LBuf, LBuf2: string;
  L256Power: Integer;
  LParts: Integer; //how many parts should we process at a time
{$ENDIF}
begin
  VErr := True;
  Result := 0;
{$IFDEF DOTNET}
  AIPaddr := System.Net.IPAddress.Parse(AIPAddress);
  try
    try
      if AIPaddr.AddressFamily = Addressfamily.InterNetwork then begin
        {$IFDEF DOTNET_2_OR_ABOVE}
        //This looks funny but it's just to circvument a warning about
        //a depreciated property in AIPaddr.  We can safely assume
        //this is an IPv4 address.
        Result := BytesToUInt32( AIPAddr.GetAddressBytes,0);
        {$ENDIF}
        {$IFDEF DOTNET_1_1}
        Result := AIPaddr.Address;
        {$ENDIF}
        VErr := False;
      end;
    except
      VErr := True;
    end;
  finally
    FreeAndNil(AIPaddr);
  end;
{$ELSE}
  // S.G. 11/8/2003: Added overflow checking disabling and change multiplys by SHLs.
  // Locally disable overflow checking so we can safely use SHL and SHR
  {$I IdOverflowCheckingOff.inc}
  L256Power := 4;
  LBuf2 := AIPAddress;
  repeat
    LBuf := Fetch(LBuf2, '.');
    if LBuf = '' then begin
      Break;
    end;
    //We do things this way because we have to treat
    //IP address parts differently than a whole number
    //and sometimes, there can be missing periods.
    if (LBuf2 = '') and (L256Power > 1) then begin
      LParts := L256Power;
      Result := Result shl (L256Power SHL 3);
    end else begin
      LParts := 1;
      Result := Result shl 8;
    end;
    if TextStartsWith(LBuf, IdHexPrefix) then begin
      //this is a hexideciaml number
      if not IsHexidecimal(Copy(LBuf, 3, MaxInt)) then begin
        Exit;
      end;
      Result := Result + IPv4MakeUInt32InRange(StrToInt64Def(LBuf, 0), LParts);
    end else begin
      if not IsNumeric(LBuf) then begin
        //There was an error meaning an invalid IP address
        Exit;
      end;
      if TextStartsWith(LBuf, '0') and IsOctal(LBuf) then begin {do not localize}
        //this is octal
        Result := Result + IPv4MakeUInt32InRange(OctalToInt64(LBuf), LParts);
      end else begin
        //this must be a decimal
        Result :=  Result + IPv4MakeUInt32InRange(StrToInt64Def(LBuf, 0), LParts);
      end;
    end;
    Dec(L256Power);
  until False;
  VErr := False;
  // Restore overflow checking
  {$I IdOverflowCheckingOn.inc}
{$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
function IPv4ToDWord(const AIPAddress: string; var VErr: Boolean): UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := IPv4ToUInt32(AIPAddress, VErr);
end;

function IPv6AddressToStr(const AValue: TIdIPv6Address): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  i: Integer;
begin
  Result := IntToHex(AValue[0], 4);
  for i := 1 to 7 do begin
    Result := Result + ':' + IntToHex(AValue[i], 4);
  end;
end;

function MakeCanonicalIPv4Address(const AAddr: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LErr: Boolean;
  LIP: UInt32;
begin
  LIP := IPv4ToUInt32(AAddr, LErr);
  if LErr then begin
    Result := '';
  end else begin
    Result := MakeUInt32IntoIPv4Address(LIP);
  end;
end;

function MakeCanonicalIPv6Address(const AAddr: string): string;
// return an empty string if the address is invalid,
// for easy checking if its an address or not.
var
  p, i: Integer;
  {$IFDEF BYTE_COMPARE_SETS}
  dots, colons: Byte;
  {$ELSE}
  dots, colons: Integer;
  {$ENDIF}
  colonpos: array[1..8] of Integer;
  dotpos: array[1..3] of Integer;
  LAddr: string;
  num: Integer;
  haddoublecolon: boolean;
  fillzeros: Integer;
begin
  Result := ''; // error
  LAddr := AAddr;
  if Length(LAddr) = 0 then begin
    Exit;
  end;
  if TextStartsWith(LAddr, ':') then begin
    LAddr := '0' + LAddr;
  end;
  if TextEndsWith(LAddr, ':') then begin
    LAddr := LAddr + '0';
  end;
  dots := 0;
  colons := 0;
  for p := 1 to Length(LAddr) do begin
    case LAddr[p] of
      '.': begin
              Inc(dots);
              if dots < 4 then begin
                dotpos[dots] := p;
              end else begin
                Exit; // error in address
              end;
            end;
      ':': begin
              Inc(colons);
              if colons < 8 then begin
                colonpos[colons] := p;
              end else begin
                Exit; // error in address
              end;
            end;
      'a'..'f',
      'A'..'F': if dots > 0 then Exit;
        // allow only decimal stuff within dotted portion, ignore otherwise
      '0'..'9': ; // do nothing
    else
      Exit; // error in address
    end; // case
  end; // for
  if not (dots in [0,3]) then begin
    Exit; // you have to write 0 or 3 dots...
  end;
  if dots = 3 then begin
    if not (colons in [2..6]) then begin
      Exit; // must not have 7 colons if we have dots
    end;
    if colonpos[colons] > dotpos[1] then begin
      Exit; // x:x:x.x:x:x is not valid
    end;
  end else begin
    if not (colons in [2..7]) then begin
      Exit; // must at least have two colons
    end;
  end;

  // now start :-)
  num := IndyStrToInt('$'+Copy(LAddr, 1, colonpos[1]-1), -1);
  if (num < 0) or (num > 65535) then begin
    Exit; // huh? odd number...
  end;
  Result := IntToHex(num, 1) + ':';

  haddoublecolon := False;
  for p := 2 to colons do begin
    if colonpos[p - 1] = colonpos[p]-1 then begin
      if haddoublecolon then begin
        Result := '';
        Exit; // only a single double-dot allowed!
      end;
      haddoublecolon := True;
      fillzeros := 8 - colons;
      if dots > 0 then begin
        Dec(fillzeros);
      end;
      for i := 1 to fillzeros do begin
        Result := Result + '0:'; {do not localize}
      end;
    end else begin
      num := IndyStrToInt('$' + Copy(LAddr, colonpos[p - 1] + 1, colonpos[p] - colonpos[p - 1] - 1), -1);
      if (num < 0) or (num > 65535) then begin
        Result := '';
        Exit; // huh? odd number...
      end;
      Result := Result + IntToHex(num,1) + ':';
    end;
  end; // end of colon separated part

  if dots = 0 then begin
    num := IndyStrToInt('$' + Copy(LAddr, colonpos[colons] + 1, MaxInt), -1);
    if (num < 0) or (num > 65535) then begin
      Result := '';
      Exit; // huh? odd number...
    end;
    Result := Result + IntToHex(num,1) + ':';
  end;

  if dots > 0 then begin
    num := IndyStrToInt(Copy(LAddr, colonpos[colons] + 1, dotpos[1] - colonpos[colons] -1),-1);
    if (num < 0) or (num > 255) then begin
      Result := '';
      Exit;
    end;
    Result := Result + IntToHex(num, 2);
    num := IndyStrToInt(Copy(LAddr, dotpos[1]+1, dotpos[2]-dotpos[1]-1),-1);
    if (num < 0) or (num > 255) then begin
      Result := '';
      Exit;
    end;
    Result := Result + IntToHex(num, 2) + ':';

    num := IndyStrToInt(Copy(LAddr, dotpos[2] + 1, dotpos[3] - dotpos[2] -1),-1);
    if (num < 0) or (num > 255) then begin
      Result := '';
      Exit;
    end;
    Result := Result + IntToHex(num, 2);
    num := IndyStrToInt(Copy(LAddr, dotpos[3] + 1, 3), -1);
    if (num < 0) or (num > 255) then begin
      Result := '';
      Exit;
    end;
    Result := Result + IntToHex(num, 2) + ':';
  end;
  SetLength(Result, Length(Result) - 1);
end;

procedure IPv6ToIdIPv6Address(const AIPAddress: String; var VAddress: TIdIPv6Address);
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LErr: Boolean;
begin
  IPv6ToIdIPv6Address(AIPAddress, VAddress, LErr);
  if LErr then begin
    raise EIdInvalidIPv6Address.CreateFmt(RSInvalidIPv6Address, [AIPAddress]);
  end;
end;

procedure IPv6ToIdIPv6Address(const AIPAddress: String; var VAddress: TIdIPv6Address; var VErr: Boolean);
var
  LAddress: string;
  I: Integer;
begin
  LAddress := MakeCanonicalIPv6Address(AIPAddress);
  VErr := (LAddress = '');
  if VErr then begin
    Exit;
  end;
  for I := 0 to 7 do begin
    VAddress[I] := IndyStrToInt('$' + Fetch(LAddress,':'), 0);
  end;
end;

function IndyMax(const AValueOne, AValueTwo: Int64): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValueOne < AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMax(const AValueOne, AValueTwo: Int32): Int32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValueOne < AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMax(const AValueOne, AValueTwo: UInt16): UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValueOne < AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

{$IFNDEF DOTNET}
// TODO: validate this with Unicode data
function MemoryPos(const ASubStr: string; MemBuff: PChar; MemorySize: Integer): Integer;
var
  LSearchLength: Integer;
  LS1: Integer;
  LChar: Char;
  LPS, LPM: PChar;
begin
  LSearchLength := Length(ASubStr);
  if (LSearchLength = 0) or (LSearchLength > (MemorySize * SizeOf(Char))) then begin
    Result := 0;
    Exit;
  end;

  LChar := PChar(Pointer(ASubStr))^; //first char
  LPS := PChar(Pointer(ASubStr))+1;//tail string
  LPM := MemBuff;
  LS1 := LSearchLength-1;
  LSearchLength := MemorySize-LS1;//MemorySize-LS+1
  if LS1 = 0 then begin //optimization for freq used LF
    while LSearchLength > 0 do begin
      if LPM^ = LChar then begin
        Result := LPM-MemBuff + 1;
        Exit;
      end;
      Inc(LPM);
      Dec(LSearchLength);
    end;//while
  end else begin
    while LSearchLength > 0 do begin
      if LPM^ = LChar then begin
        Inc(LPM);
        if CompareMem(LPM, LPS, LS1 * SizeOf(Char)) then begin
          Result := LPM - MemBuff;
          Exit;
        end;
      end else begin
        Inc(LPM);
      end;
      Dec(LSearchLength);
    end;
  end;
  Result := 0;
end;
{$ENDIF}

function IndyMin(const AValueOne, AValueTwo: Int32): Int32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValueOne > AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMin(const AValueOne, AValueTwo: Int64): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValueOne > AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function IndyMin(const AValueOne, AValueTwo: UInt16): UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AValueOne > AValueTwo then begin
    Result := AValueTwo;
  end else begin
    Result := AValueOne;
  end;
end;

function PosIdx(const ASubStr, AStr: string; AStartPos: UInt32): UInt32;
{$IFDEF DOTNET}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
  // use best register allocation on Win32
  function FindStr(ALStartPos, EndPos: UInt32; StartChar: Char; const ALStr: string): UInt32;
  begin
    for Result := ALStartPos to EndPos do begin
      if ALStr[Result] = StartChar then begin
        Exit;
      end;
    end;
    Result := 0;
  end;

  // use best register allocation on Win32
  function FindNextStr(ALStartPos, EndPos: UInt32; const ALStr, ALSubStr: string): UInt32;
  begin
    for Result := ALStartPos + 1 to EndPos do begin
      if ALStr[Result] <> ALSubStr[Result - ALStartPos + 1] then begin
        Exit;
      end;
    end;
    Result := 0;
  end;

var
  StartChar: Char;
  LenSubStr, LenStr: UInt32;
  EndPos: UInt32;
{$ENDIF}
begin
  if AStartPos = 0 then begin
    AStartPos := 1;
  end;
  {$IFDEF DOTNET}
  Result := AStr.IndexOf(ASubStr, AStartPos-1) + 1;
  {$ELSE}
  Result := 0;
  LenSubStr := Length(ASubStr);
  LenStr := Length(AStr);
  if (LenSubStr = 0) or (AStr = '') or (LenSubStr > (LenStr - (AStartPos - 1))) then begin
    Exit;
  end;
  StartChar := ASubStr[1];
  EndPos := LenStr - LenSubStr + 1;
  if LenSubStr = 1 then begin
    Result := FindStr(AStartPos, EndPos, StartChar, AStr)
  end else
  begin
    repeat
      Result := FindStr(AStartPos, EndPos, StartChar, AStr);
      if Result = 0 then begin
        Break;
      end;
      AStartPos := Result;
      Result := FindNextStr(Result, AStartPos + LenSubStr - 1, AStr, ASubStr);
      if Result = 0 then
      begin
        Result := AStartPos;
        Exit;
      end;
      Inc(AStartPos);
    until False;
  end;
  {$ENDIF}
end;

function SBPos(const Substr, S: string): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // Necessary because of "Compiler magic"
  Result := Pos(Substr, S);
end;

{$IFNDEF DOTNET}
function SBStrScan(Str: PChar; Chr: Char): PChar;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := SysUtils.StrScan(Str, Chr);
end;
{$ENDIF}

{$IFNDEF DOTNET}
//Don't rename this back to AnsiPos because that conceals a symbol in Windows
function InternalAnsiPos(const Substr, S: string): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := SysUtils.AnsiPos(Substr, S);
end;

function InternalAnsiStrScan(Str: PChar; Chr: Char): PChar;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := SysUtils.AnsiStrScan(Str, Chr);
end;
{$ENDIF}

{$UNDEF USE_TTHREAD_PRIORITY_PROP}
{$IFDEF DOTNET}
  {$DEFINE USE_TTHREAD_PRIORITY_PROP}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE USE_TTHREAD_PRIORITY_PROP}
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF USE_VCL_POSIX}
    // TODO: does this apply?
    {.$DEFINE USE_TTHREAD_PRIORITY_PROP}
  {$ENDIF}
  {$IFDEF KYLIXCOMPAT} // TODO: use KYLIXCOMPAT_OR_VCL_POSIX instead?
    {$IFNDEF INT_THREAD_PRIORITY}
      {$DEFINE USE_TTHREAD_PRIORITY_PROP}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

procedure IndySetThreadPriority(AThread: TThread; const APriority: TIdThreadPriority;
  const APolicy: Integer = -MaxInt);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF USE_TTHREAD_PRIORITY_PROP}
  AThread.Priority := APriority;
  {$ELSE}
    {$IFDEF UNIX}
      // Linux only allows root to adjust thread priorities, so we just ignore this call in Linux?
      // actually, why not allow it if root
      // and also allow setting *down* threadpriority (anyone can do that)
      // note that priority is called "niceness" and positive is lower priority
      {$IFDEF KYLIXCOMPAT} // TODO: use KYLIXCOMPAT_OR_VCL_POSIX instead?
   if (getpriority(PRIO_PROCESS, 0) < APriority) or (geteuid = 0) then begin
     setpriority(PRIO_PROCESS, 0, APriority);
   end;
      {$ELSE}
        {$IFDEF USE_BASEUNIX}
  if (fpgetpriority(PRIO_PROCESS, 0) < cint(APriority)) or (fpgeteuid = 0) then begin
    fpsetpriority(PRIO_PROCESS, 0, cint(APriority));
  end;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

procedure IndySleep(ATime: UInt32);
{$IFDEF USE_VCL_POSIX}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LTime: TimeVal;
{$ELSE}
  {$IFDEF UNIX}
var
  LTime: TTimeVal;
  {$ELSE}
    {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}

  Thread.Sleep(ATime);

  {$ELSE}
    {$IFDEF WINDOWS}

  Windows.Sleep(ATime);

    {$ELSE}
      {$IFDEF UNIX}

    // *nix: Is there any reason for not using nanosleep() instead?

    // what if the user just calls sleep? without doing anything...
    // cannot use GStack.WSSelectRead(nil, ATime)
    // since no readsocketlist exists to get the fdset
  LTime.tv_sec := ATime div 1000;
  LTime.tv_usec := (ATime mod 1000) * 1000;
        {$IFDEF USE_VCL_POSIX}
  select(0, nil, nil, nil, @LTime);
        {$ELSE}
          {$IFDEF KYLIXCOMPAT}
  Libc.Select(0, nil, nil, nil, @LTime);
          {$ELSE}
            {$IFDEF USE_BASEUNIX}
  fpSelect(0, nil, nil, nil, @LTime);
            {$ELSE}
              {$message error select is not called on this platform!}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}

      {$ELSE}
        {$message error IndySleep is not implemented on this platform!}
      {$ENDIF}

    {$ENDIF}
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
procedure SplitColumnsNoTrim(const AData: string; AStrings: TStrings; const ADelim: string = ' ');
{$I IdDeprecatedImplBugOn.inc}
begin
  SplitDelimitedString(AData, AStrings, False, ADelim{$IFNDEF USE_OBJECT_ARC}, True{$ENDIF});
end;

{$I IdDeprecatedImplBugOff.inc}
procedure SplitColumns(const AData: string; AStrings: TStrings; const ADelim: string = ' ');
{$I IdDeprecatedImplBugOn.inc}
begin
  SplitDelimitedString(AData, AStrings, True, ADelim{$IFNDEF USE_OBJECT_ARC}, True{$ENDIF});
end;

procedure SplitDelimitedString(const AData: string; AStrings: TStrings; ATrim: Boolean;
  const ADelim: string = ' '{$IFNDEF USE_OBJECT_ARC}; AIncludePositions: Boolean = False{$ENDIF});
var
  i: Integer;
  LData: string;
  LDelim: Integer; //delim len
  LLeft: string;
  LLastPos, LLeadingSpaceCnt: PtrInt;
begin
  Assert(Assigned(AStrings));
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    LDelim := Length(ADelim);
    LLastPos := 1;

    if ATrim then begin
      LData := Trim(AData);
      if LData = '' then begin //if WhiteStr
        Exit;
      end;

      LLeadingSpaceCnt := 0;
      while AData[LLeadingSpaceCnt + 1] <= #32 do begin
        Inc(LLeadingSpaceCnt);
      end;

      i := Pos(ADelim, LData);
      while I > 0 do begin
        LLeft := Copy(LData, LLastPos, I - LLastPos); //'abc d' len:=i(=4)-1    {Do not Localize}
        if LLeft > '' then begin    {Do not Localize}
          {$IFNDEF USE_OBJECT_ARC}
          if AIncludePositions then begin
            AStrings.AddObject(Trim(LLeft), TObject(LLastPos + LLeadingSpaceCnt));
          end else
          {$ENDIF}
          begin
            AStrings.Add(Trim(LLeft));
          end;
        end;
        LLastPos := I + LDelim; //first char after Delim
        i := PosIdx(ADelim, LData, LLastPos);
      end;//while found
      if LLastPos <= Length(LData) then begin
        {$IFNDEF USE_OBJECT_ARC}
        if AIncludePositions then begin
          AStrings.AddObject(Trim(Copy(LData, LLastPos, MaxInt)), TObject(LLastPos + LLeadingSpaceCnt));
        end else
        {$ENDIF}
        begin
          AStrings.Add(Trim(Copy(LData, LLastPos, MaxInt)));
        end;
      end;
    end else
    begin
      i := Pos(ADelim, AData);
      while I > 0 do begin
        LLeft := Copy(AData, LLastPos, I - LLastPos); //'abc d' len:=i(=4)-1    {Do not Localize}
        if LLeft <> '' then begin    {Do not Localize}
          {$IFNDEF USE_OBJECT_ARC}
          if AIncludePositions then begin
            AStrings.AddObject(LLeft, TObject(LLastPos));
          end else
          {$ENDIF}
          begin
            AStrings.Add(LLeft);
          end;
        end;
        LLastPos := I + LDelim; //first char after Delim
        i := PosIdx(ADelim, AData, LLastPos);
      end;
      if LLastPos <= Length(AData) then begin
        {$IFNDEF USE_OBJECT_ARC}
        if AIncludePositions then begin
          AStrings.AddObject(Copy(AData, LLastPos, MaxInt), TObject(LLastPos));
        end else
        {$ENDIF}
        begin
          AStrings.Add(Copy(AData, LLastPos, MaxInt));
        end;
      end;
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

{$IFDEF USE_OBJECT_ARC}
constructor TIdStringPosition.Create(const AValue: String; const APosition: Integer);
begin
  Value := AValue;
  Position := APosition;
end;

procedure SplitDelimitedString(const AData: string; AStrings: TIdStringPositionList;
  ATrim: Boolean; const ADelim: string = ' ');
var
  i: Integer;
  LData: string;
  LDelim: Integer; //delim len
  LLeft: string;
  LLastPos, LLeadingSpaceCnt: Integer;
begin
  Assert(Assigned(AStrings));
  AStrings.Clear;
  LDelim := Length(ADelim);
  LLastPos := 1;

  if ATrim then begin
    LData := Trim(AData);
    if LData = '' then begin //if WhiteStr
      Exit;
    end;

    LLeadingSpaceCnt := 0;
    while AData[LLeadingSpaceCnt + 1] <= #32 do begin
      Inc(LLeadingSpaceCnt);
    end;

    i := Pos(ADelim, LData);
    while I > 0 do begin
      LLeft := Copy(LData, LLastPos, I - LLastPos); //'abc d' len:=i(=4)-1    {Do not Localize}
      if LLeft > '' then begin    {Do not Localize}
        AStrings.Add(TIdStringPosition.Create(Trim(LLeft), LLastPos + LLeadingSpaceCnt));
      end;
      LLastPos := I + LDelim; //first char after Delim
      i := PosIdx(ADelim, LData, LLastPos);
    end;//while found
    if LLastPos <= Length(LData) then begin
      AStrings.Add(TIdStringPosition.Create(Trim(Copy(LData, LLastPos, MaxInt)), LLastPos + LLeadingSpaceCnt));
    end;
  end else
  begin
    i := Pos(ADelim, AData);
    while I > 0 do begin
      LLeft := Copy(AData, LLastPos, I - LLastPos); //'abc d' len:=i(=4)-1    {Do not Localize}
      if LLeft <> '' then begin    {Do not Localize}
        AStrings.Add(TIdStringPosition.Create(LLeft, LLastPos));
      end;
      LLastPos := I + LDelim; //first char after Delim
      i := PosIdx(ADelim, AData, LLastPos);
    end;
    if LLastPos <= Length(AData) then begin
      AStrings.Add(TIdStringPosition.Create(Copy(AData, LLastPos, MaxInt), LLastPos));
    end;
  end;
end;
{$ENDIF}

{$IFDEF DOTNET}
procedure SetThreadName(const AName: string; AThread: System.Threading.Thread = nil);
begin
  if AThread = nil then begin
    AThread := System.Threading.Thread.CurrentThread;
  end;
  // cannot rename a previously-named thread
  if AThread.Name = nil then begin
    AThread.Name := AName;
  end;
end;
{$ELSE}
procedure SetThreadName(const AName: string; AThreadID: UInt32 = $FFFFFFFF);
  {$IFDEF HAS_NAMED_THREADS}
    {$IFDEF HAS_TThread_NameThreadForDebugging}
      {$IFDEF USE_INLINE}inline;{$ENDIF}
    {$ELSE}
      {$IFDEF WINDOWS}
const
  MS_VC_EXCEPTION = $406D1388;
type
  TThreadNameInfo = record
    RecType: UInt32;    // Must be 0x1000
    Name: PAnsiChar;    // Pointer to name (in user address space)
    ThreadID: UInt32;   // Thread ID (-1 indicates caller thread)
    Flags: UInt32;      // Reserved for future use. Must be zero
  end;
var
        {$IFDEF STRING_IS_UNICODE}
  LName: AnsiString;
        {$ENDIF}
  LThreadNameInfo: TThreadNameInfo;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
begin
  {$IFDEF HAS_NAMED_THREADS}
    {$IFDEF HAS_TThread_NameThreadForDebugging}
  TThread.NameThreadForDebugging(
      {$IFDEF HAS_AnsiString}
    AnsiString(AName) // explicit convert to Ansi
      {$ELSE}
    AName
      {$ENDIF},
    AThreadID
  );
    {$ELSE}
      {$IFDEF WINDOWS}
        {$IFDEF STRING_IS_UNICODE}
  LName := AnsiString(AName); // explicit convert to Ansi
        {$ENDIF}
  LThreadNameInfo.RecType := $1000;
  LThreadNameInfo.Name := PAnsiChar({$IFDEF STRING_IS_UNICODE}LName{$ELSE}AName{$ENDIF});
  LThreadNameInfo.ThreadID := AThreadID;
  LThreadNameInfo.Flags := 0;
  try
    // This is a wierdo Windows way to pass the info in
    RaiseException(MS_VC_EXCEPTION, 0, SizeOf(LThreadNameInfo) div SizeOf(UInt32),
      PDWord(@LThreadNameInfo));
  except
  end;
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  // Do nothing. No support in this compiler for it.
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF DOTNET}
  {$IFNDEF DOTNET_2_OR_ABOVE}

{ TEvent }

constructor TEvent.Create(EventAttributes: IntPtr; ManualReset, InitialState: Boolean; const Name: string);
begin
  inherited Create;
  // Name not used
  if ManualReset then begin
    FEvent := ManualResetEvent.Create(InitialState);
  end else begin
    FEvent := AutoResetEvent.Create(InitialState);
  end;
end;

constructor TEvent.Create;
begin
  Create(nil, True, False, '');    {Do not Localize}
end;

destructor TEvent.Destroy;
begin
  if Assigned(FEvent) then begin
    FEvent.Close;
  end;
  FreeAndNil(FEvent);
  inherited Destroy;
end;

procedure TEvent.SetEvent;
begin
  if FEvent is ManualResetEvent then begin
    ManualResetEvent(FEvent).&Set;
  end else begin
    AutoResetEvent(FEvent).&Set;
  end;
end;

procedure TEvent.ResetEvent;
begin
  if FEvent is ManualResetEvent then begin
    ManualResetEvent(FEvent).Reset;
  end else begin
    AutoResetEvent(FEvent).Reset;
  end;
end;

function TEvent.WaitFor(Timeout: UInt32): TWaitResult;
var
  Passed: Boolean;
begin
  try
    if Timeout = INFINITE then begin
      Passed := FEvent.WaitOne;
    end else begin
      Passed := FEvent.WaitOne(Timeout, True);
    end;
    if Passed then begin
      Result := wrSignaled;
    end else begin
      Result := wrTimeout;
    end;
  except
    Result := wrError;
  end;
end;

{ TCriticalSection }

procedure TCriticalSection.Acquire;
begin
  Enter;
end;

procedure TCriticalSection.Release;
begin
  Leave;
end;

function TCriticalSection.TryEnter: Boolean;
begin
  Result := System.Threading.Monitor.TryEnter(Self);
end;

procedure TCriticalSection.Enter;
begin
  System.Threading.Monitor.Enter(Self);
end;

procedure TCriticalSection.Leave;
begin
  System.Threading.Monitor.Exit(Self);
end;
   {$ENDIF}
{$ENDIF}

{ TIdLocalEvent }

constructor TIdLocalEvent.Create(const AInitialState: Boolean = False; const AManualReset: Boolean = False);
begin
  inherited Create(nil, AManualReset, AInitialState, '');    {Do not Localize}
end;

function TIdLocalEvent.WaitForEver: TWaitResult;
begin
  Result := WaitFor(Infinite);
end;

procedure ToDo(const AMsg: string);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  raise EIdException.Create(AMsg);
end;

// RLebeau: the following three functions are utility functions
// that determine the usable amount of data in various buffer types.
// There are many operations in Indy that allow the user to specify
// data sizes, or to have Indy calculate it.  So these functions
// help reduce code duplication.

function IndyLength(const ABuffer: String; const ALength: Integer = -1; const AIndex: Integer = 1): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LAvailable: Integer;
begin
  Assert(AIndex >= 1);
  LAvailable := IndyMax(Length(ABuffer)-AIndex+1, 0);
  if ALength < 0 then begin
    Result := LAvailable;
  end else begin
    Result := IndyMin(LAvailable, ALength);
  end;
end;

function IndyLength(const ABuffer: TIdBytes; const ALength: Integer = -1; const AIndex: Integer = 0): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LAvailable: Integer;
begin
  Assert(AIndex >= 0);
  LAvailable := IndyMax(Length(ABuffer)-AIndex, 0);
  if ALength < 0 then begin
    Result := LAvailable;
  end else begin
    Result := IndyMin(LAvailable, ALength);
  end;
end;

function IndyLength(const ABuffer: TStream; const ALength: TIdStreamSize = -1): TIdStreamSize; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LAvailable: TIdStreamSize;
begin
  LAvailable := IndyMax(ABuffer.Size - ABuffer.Position, 0);
  if ALength < 0 then begin
    Result := LAvailable;
  end else begin
    Result := IndyMin(LAvailable, ALength);
  end;
end;

const
  wdays: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'); {do not localize}
  monthnames: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', {do not localize}
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'); {do not localize}

{$IFDEF HAS_TFormatSettings}
//Delphi5 does not have TFormatSettings
//this should be changed to a singleton?
function GetEnglishSetting: TFormatSettings;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result.CurrencyFormat := $00; // 0 = '$1'
  Result.NegCurrFormat := $00; //0 = '($1)'
  Result.CurrencyString := '$';                               {do not localize}
  Result.CurrencyDecimals := 2;

  Result.ThousandSeparator := ',';                            {do not localize}
  Result.DecimalSeparator := '.';                             {do not localize}

  Result.DateSeparator := '/';                                {do not localize}
  Result.ShortDateFormat := 'M/d/yyyy';                       {do not localize}
  Result.LongDateFormat := 'dddd, MMMM dd, yyyy';             {do not localize}

  Result.TimeSeparator := ':';                                {do not localize}
  Result.TimeAMString := 'AM';                                {do not localize}
  Result.TimePMString := 'PM';                                {do not localize}
  Result.LongTimeFormat := 'h:mm:ss AMPM';                    {do not localize}
  Result.ShortTimeFormat := 'h:mm AMPM';                      {do not localize}

  Result.ShortMonthNames[1] := monthnames[1]; //'Jan';
  Result.ShortMonthNames[2] := monthnames[2]; //'Feb';
  Result.ShortMonthNames[3] := monthnames[3]; //'Mar';
  Result.ShortMonthNames[4] := monthnames[4]; //'Apr';
  Result.ShortMonthNames[5] := monthnames[5]; //'May';
  Result.ShortMonthNames[6] := monthnames[6]; //'Jun';
  Result.ShortMonthNames[7] := monthnames[7]; //'Jul';
  Result.ShortMonthNames[8] := monthnames[8]; //'Aug';
  Result.ShortMonthNames[9] := monthnames[9]; //'Sep';
  Result.ShortMonthNames[10] := monthnames[10];// 'Oct';
  Result.ShortMonthNames[11] := monthnames[11]; //'Nov';
  Result.ShortMonthNames[12] := monthnames[12]; //'Dec';

  Result.LongMonthNames[1] := 'January';                      {do not localize}
  Result.LongMonthNames[2] := 'February';                     {do not localize}
  Result.LongMonthNames[3] := 'March';                        {do not localize}
  Result.LongMonthNames[4] := 'April';                        {do not localize}
  Result.LongMonthNames[5] := 'May';                          {do not localize}
  Result.LongMonthNames[6] := 'June';                         {do not localize}
  Result.LongMonthNames[7] := 'July';                         {do not localize}
  Result.LongMonthNames[8] := 'August';                       {do not localize}
  Result.LongMonthNames[9] := 'September';                    {do not localize}
  Result.LongMonthNames[10] := 'October';                     {do not localize}
  Result.LongMonthNames[11] := 'November';                    {do not localize}
  Result.LongMonthNames[12] := 'December';                    {do not localize}

  Result.ShortDayNames[1] := wdays[1]; //'Sun';
  Result.ShortDayNames[2] := wdays[2]; //'Mon';
  Result.ShortDayNames[3] := wdays[3]; //'Tue';
  Result.ShortDayNames[4] := wdays[4]; //'Wed';
  Result.ShortDayNames[5] := wdays[5]; //'Thu';
  Result.ShortDayNames[6] := wdays[6]; //'Fri';
  Result.ShortDayNames[7] := wdays[7]; //'Sat';

  Result.LongDayNames[1] := 'Sunday';                         {do not localize}
  Result.LongDayNames[2] := 'Monday';                         {do not localize}
  Result.LongDayNames[3] := 'Tuesday';                        {do not localize}
  Result.LongDayNames[4] := 'Wednesday';                      {do not localize}
  Result.LongDayNames[5] := 'Thursday';                       {do not localize}
  Result.LongDayNames[6] := 'Friday';                         {do not localize}
  Result.LongDayNames[7] := 'Saturday';                       {do not localize}

  Result.ListSeparator := ',';                                {do not localize}
end;
{$ENDIF}

// RLebeau 10/24/2008: In the RTM release of Delphi/C++Builder 2009, the
// overloaded version of SysUtils.Format() that has a TFormatSettings parameter
// has an internal bug that causes an EConvertError exception when UnicodeString
// parameters greater than 4094 characters are passed to it.  Refer to QC #67934
// for details.  The bug is fixed in 2009 Update 1.  For RTM, call FormatBuf()
// directly to work around the problem...
function IndyFormat(const AFormat: string; const Args: array of const): string;
{$IFNDEF DOTNET}
  {$IFDEF HAS_TFormatSettings}
var
  EnglishFmt: TFormatSettings;
    {$IFDEF BROKEN_FmtStr}
  Len, BufLen: Integer;
  Buffer: array[0..4095] of Char;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  // RLebeau 10/29/09: temporary workaround until we figure out how to use
  // SysUtils.FormatBuf() correctly under .NET in D2009 RTM...
  Result := SysUtils.Format(AFormat, Args);
  {$ELSE}
    {$IFDEF HAS_TFormatSettings}
  EnglishFmt := GetEnglishSetting;
      {$IFDEF BROKEN_FmtStr}
  BufLen := Length(Buffer);
  if Length(AFormat) < (Length(Buffer) - (Length(Buffer) div 4)) then
  begin
    Len := SysUtils.FormatBuf(Buffer, Length(Buffer) - 1, Pointer(AFormat)^,
      Length(AFormat), Args, EnglishFmt);
  end else
  begin
    BufLen := Length(AFormat);
    Len := BufLen;
  end;
  if Len >= BufLen - 1 then
  begin
    while Len >= BufLen - 1 do
    begin
      Inc(BufLen, BufLen);
      Result := '';          // prevent copying of existing data, for speed
      SetLength(Result, BufLen);
      Len := SysUtils.FormatBuf(PChar(Result), BufLen - 1, Pointer(AFormat)^,
        Length(AFormat), Args, EnglishFmt);
    end;
    SetLength(Result, Len);
  end else
  begin
    SetString(Result, Buffer, Len);
  end;
      {$ELSE}
  Result := SysUtils.Format(AFormat, Args, EnglishFmt);
      {$ENDIF}
    {$ELSE}
  //Is there a way to get delphi5 to use locale in format? something like:
  //  SetThreadLocale(TheNewLocaleId);
  //  GetFormatSettings;
  //  Application.UpdateFormatSettings := False; //needed?
  //  format()
  //  set locale back to prior
  Result := SysUtils.Format(AFormat, Args);
    {$ENDIF}
  {$ENDIF}
end;

function DateTimeGMTToHttpStr(const GMTValue: TDateTime) : String;
// should adhere to RFC 2616
var
  wDay, wMonth, wYear: Word;
begin
  DecodeDate(GMTValue, wYear, wMonth, wDay);
  Result := IndyFormat('%s, %.2d %s %.4d %s %s',    {do not localize}
                   [wdays[DayOfWeek(GMTValue)], wDay, monthnames[wMonth],
                    wYear, FormatDateTime('HH":"nn":"ss',GMTValue), 'GMT']);  {do not localize}
end;

function DateTimeGMTToCookieStr(const GMTValue: TDateTime; const AUseNetscapeFmt: Boolean = True) : String;
var
  wDay, wMonth, wYear: Word;
  LDelim: Char;
begin
  DecodeDate(GMTValue, wYear, wMonth, wDay);
  // RLebeau: cookie draft-23 requires HTTP servers to format an Expires value as follows:
  //
  // Wdy, DD Mon YYYY HH:MM:SS GMT
  //
  // However, Netscape style formatting, which RFCs 2109 and 2965 allow
  // (but draft-23 obsoletes), are more common:
  //
  // Wdy, DD-Mon-YY HH:MM:SS GMT   (original)
  // Wdy, DD-Mon-YYYY HH:MM:SS GMT (RFC 1123)
  //
  if AUseNetscapeFmt then begin
    LDelim := '-';    {do not localize}
  end else begin
    LDelim := ' ';    {do not localize}
  end;
  Result := IndyFormat('%s, %.2d%s%s%s%.4d %s %s',    {do not localize}
                   [wdays[DayOfWeek(GMTValue)], wDay, LDelim, monthnames[wMonth], LDelim, wYear,
                   FormatDateTime('HH":"nn":"ss',GMTValue), 'GMT']);  {do not localize}
end;

function DateTimeGMTToImapStr(const GMTValue: TDateTime) : String;
var
  wDay, wMonth, wYear: Word;
  LDay: String;
begin
  DecodeDate(GMTValue, wYear, wMonth, wDay);
  LDay := IntToStr(wDay);
  if Length(LDay) < 2 then begin
    LDay := ' ' + LDay; // NOTE: space NOT zero!
  end;
  Result := IndyFormat('%s-%s-%d %s %s',    {do not localize}
                   [LDay, monthnames[wMonth], wYear, FormatDateTime('HH":"nn":"ss',GMTValue), {do not localize}
                    '+0000']); {do not localize}
end;

function LocalDateTimeToHttpStr(const Value: TDateTime) : String;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := DateTimeGMTToHttpStr(Value - OffsetFromUTC);
end;

function LocalDateTimeToCookieStr(const Value: TDateTime; const AUseNetscapeFmt: Boolean = True) : String;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := DateTimeGMTToCookieStr(Value - OffsetFromUTC, AUseNetscapeFmt);
end;

function LocalDateTimeToImapStr(const Value: TDateTime) : String;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := DateTimeGMTToImapStr(Value - OffsetFromUTC);
end;

{$I IdDeprecatedImplBugOff.inc}
function DateTimeToInternetStr(const Value: TDateTime; const AUseGMTStr : Boolean = False) : String;
{$I IdDeprecatedImplBugOn.inc}
begin
  Result := LocalDateTimeToGMT(Value, AUseGMTStr);
end;

{This should never be localized}
function LocalDateTimeToGMT(const Value: TDateTime; const AUseGMTStr: Boolean = False) : String;
var
  wDay, wMonth, wYear: Word;
begin
  DecodeDate(Value, wYear, wMonth, wDay);
  Result := IndyFormat('%s, %d %s %d %s %s',    {do not localize}
                   [wdays[DayOfWeek(Value)], wDay, monthnames[wMonth],
                    wYear, FormatDateTime('HH":"nn":"ss', Value), {do not localize}
                    UTCOffsetToStr(OffsetFromUTC, AUseGMTStr)]);
end;

{$I IdDeprecatedImplBugOff.inc}
function DateTimeToGmtOffSetStr(ADateTime: TDateTime; const AUseGMTStr: Boolean = False): string;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := UTCOffsetToStr(ADateTime, AUseGMTStr);
end;

function OffsetFromUTC: TDateTime;
{$IFDEF DOTNET}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
  {$IFDEF WINDOWS}
var
  iBias: Integer;
  tmez: TTimeZoneInformation;
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF USE_VCL_POSIX}
var
  T : Time_t;
  TV : TimeVal;
  UT : tm;
      {$ELSE}
        {$IFDEF KYLIXCOMPAT}
var
  T : Time_T;
  TV : TTimeVal;
  UT : TUnixTime;
        {$ELSE}
          {$IFDEF USE_BASEUNIX}
 var
   timeval: TTimeVal;
   timezone: TTimeZone;
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.Timezone.CurrentTimezone.GetUTCOffset(DateTime.FromOADate(Now)).TotalDays;
  {$ELSE}
    {$IFDEF WINDOWS}
  case GetTimeZoneInformation({$IFDEF WINCE}@{$ENDIF}tmez) of
    TIME_ZONE_ID_INVALID  :
      raise EIdFailedToRetreiveTimeZoneInfo.Create(RSFailedTimeZoneInfo);
    TIME_ZONE_ID_UNKNOWN  :
       iBias := tmez.Bias;
    TIME_ZONE_ID_DAYLIGHT : begin
      iBias := tmez.Bias;
      if tmez.DaylightDate.wMonth <> 0 then begin
        iBias := iBias + tmez.DaylightBias;
      end;
    end;
    TIME_ZONE_ID_STANDARD : begin
      iBias := tmez.Bias;
      if tmez.StandardDate.wMonth <> 0 then begin
        iBias := iBias + tmez.StandardBias;
      end;
    end
  else
    begin
      raise EIdFailedToRetreiveTimeZoneInfo.Create(RSFailedTimeZoneInfo);
    end;
  end;
  {We use ABS because EncodeTime will only accept positive values}
  Result := EncodeTime(Abs(iBias) div 60, Abs(iBias) mod 60, 0, 0);
  {The GetTimeZone function returns values oriented towards converting
   a GMT time into a local time.  We wish to do the opposite by returning
   the difference between the local time and GMT.  So I just make a positive
   value negative and leave a negative value as positive}
  if iBias > 0 then begin
    Result := 0.0 - Result;
  end;
    {$ELSE}
      {$IFDEF UNIX}

        {$IFDEF KYLIXCOMPAT_OR_VCL_POSIX}
  {from http://edn.embarcadero.com/article/27890 but without multiplying the Result by -1}

  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r({$IFDEF KYLIXCOMPAT}@{$ENDIF}T, UT);
  Result := UT.{$IFDEF KYLIXCOMPAT}__tm_gmtoff{$ELSE}tm_gmtoff{$ENDIF} / 60 / 60 / 24;
        {$ELSE}
          {$IFDEF USE_BASEUNIX}
  fpGetTimeOfDay (@TimeVal, @TimeZone);
  Result := -1 * (timezone.tz_minuteswest / 60 / 24);
          {$ELSE}
  {$message error gettimeofday is not called on this platform!}
  Result := GOffsetFromUTC;
          {$ENDIF}
        {$ENDIF}

      {$ELSE}
  Result := GOffsetFromUTC;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function UTCOffsetToStr(const AOffset: TDateTime; const AUseGMTStr: Boolean = False): string;
var
  AHour, AMin, ASec, AMSec: Word;
  {$IFDEF STRING_IS_IMMUTABLE}
  LSB: TIdStringBuilder;
  {$ENDIF}
begin
  if (AOffset = 0.0) and AUseGMTStr then
  begin
    Result := 'GMT'; {do not localize}
  end else
  begin
    DecodeTime(AOffset, AHour, AMin, ASec, AMSec);
    {$IFDEF STRING_IS_IMMUTABLE}
    LSB := TIdStringBuilder.Create(5);
    LSB.Append(IndyFormat(' %0.2d%0.2d', [AHour, AMin])); {do not localize}
    if AOffset < 0.0 then begin
      LSB[0] := '-'; {do not localize}
    end else begin
      LSB[0] := '+'; {do not localize}
    end;
    Result := LSB.ToString;
    {$ELSE}
    Result := IndyFormat(' %0.2d%0.2d', [AHour, AMin]); {do not localize}
    if AOffset < 0.0 then begin
      Result[1] := '-'; {do not localize}
    end else begin
      Result[1] := '+';  {do not localize}
    end;
    {$ENDIF}
  end;
end;

function IndyIncludeTrailingPathDelimiter(const S: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_SysUtils_IncludeExcludeTrailingPathDelimiter}
  Result := SysUtils.IncludeTrailingPathDelimiter(S);
  {$ELSE}
  Result := SysUtils.IncludeTrailingBackslash(S);
  {$ENDIF}
end;

function IndyExcludeTrailingPathDelimiter(const S: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_SysUtils_IncludeExcludeTrailingPathDelimiter}
  Result := SysUtils.ExcludeTrailingPathDelimiter(S);
  {$ELSE}
  Result := SysUtils.ExcludeTrailingBackslash(S);
  {$ENDIF}
end;

function StringsReplace(const S: String; const OldPattern, NewPattern: array of string): string;
var
  i : Integer;
begin
  // TODO: re-write this to not use ReplaceAll() in a loop anymore.  If
  // OldPattern contains multiple strings, a string appearing later in the
  // list may be replaced multiple times by accident if it appears in the
  // Result of an earlier string replacement.
  Result := s;
  for i := Low(OldPattern) to High(OldPattern) do begin
    Result := ReplaceAll(Result, OldPattern[i], NewPattern[i]);
  end;
end;

{$IFNDEF DOTNET}
  {$IFNDEF HAS_PosEx}
function PosEx(const SubStr, S: string; Offset: Integer): Integer;
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PChar;
begin
  Result := 0;
  if SubStr = '' then begin
    Exit;
  end;

  { Calculate the number of possible iterations. Not valid if Offset < 1. }
  LIterCnt := Length(S) - Offset - Length(SubStr) + 1;

  { Only continue if the number of iterations is positive or zero (there is space to check) }
  if (Offset > 0) and (LIterCnt >= 0) then
  begin
    L := Length(SubStr);
    PSubStr := PChar(SubStr);
    PS := PChar(S);
    Inc(PS, Offset - 1);

    for I := 0 to LIterCnt do
    begin
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        if PS[I + J] = PSubStr[J] then begin
          Inc(J);
        end else begin
          J := -1;
        end;
      end;
      if J >= L then begin
        Result := I + Offset;
        Exit;
      end;
    end;
  end;
end;
  {$ENDIF}
{$ENDIF}

function ReplaceAll(const S: String; const OldPattern, NewPattern: String): String;
var
  I, PatLen: Integer;
  {$IFDEF DOTNET}
  J: Integer;
  {$ELSE}
  NumBytes: Integer;
  {$ENDIF}
begin
  PatLen := Length(OldPattern);
  if Length(NewPattern) = PatLen then begin
    Result := S;
    I := Pos(OldPattern, Result);
    if I > 0 then begin
      UniqueString(Result);
      {$IFNDEF DOTNET}
      NumBytes := PatLen * SizeOf(Char);
      {$ENDIF}
      repeat
        {$IFDEF DOTNET}
        for J := 1 to PatLen do begin
          Result[I+J-1] := NewPattern[J];
        end;
        {$ELSE}
        Move(PChar(NewPattern)^, Result[I], NumBytes);
        {$ENDIF}
        I := PosEx(OldPattern, Result, I + PatLen);
     until I = 0;
    end;
  end else begin
    Result := SysUtils.StringReplace(S, OldPattern, NewPattern, [rfReplaceAll]);
  end;
end;

function ReplaceOnlyFirst(const S, OldPattern, NewPattern: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := SysUtils.StringReplace(s, OldPattern, NewPattern, []);
end;

function IndyStrToInt(const S: string): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := StrToInt(Trim(S));
end;

function IndyStrToInt(const S: string; ADefault: Integer): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := StrToIntDef(Trim(S), ADefault);
end;

function CompareDate(const D1, D2: TDateTime): Integer;
var
  LTM1, LTM2 : TTimeStamp;
begin
  LTM1 := DateTimeToTimeStamp(D1);
  LTM2 := DateTimeToTimeStamp(D2);
  if LTM1.Date = LTM2.Date then begin
    if LTM1.Time < LTM2.Time then begin
      Result := -1;
    end
    else if LTM1.Time > LTM2.Time then begin
      Result := 1;
    end
    else begin
      Result := 0;
    end;
  end
  else if LTM1.Date > LTM2.Date then begin
    Result := 1;
  end
  else begin
    Result := -1;
  end;
end;

function AddMSecToTime(const ADateTime: TDateTime; const AMSec: Integer): TDateTime;
{$IFDEF HAS_UNIT_DateUtils}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
var
  LTM : TTimeStamp;
{$ENDIF}
begin
  {$IFDEF HAS_UNIT_DateUtils}
  Result := DateUtils.IncMilliSecond(ADateTime, AMSec);
  {$ELSE}
  LTM := DateTimeToTimeStamp(ADateTime);
  LTM.Time := LTM.Time + AMSec;
  Result := TimeStampToDateTime(LTM);
  {$ENDIF}
end;

function IndyFileAge(const AFileName: string): TDateTime;
{$IFDEF HAS_2PARAM_FileAge}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
var
  LAge: Integer;
{$ENDIF}
begin
  {$IFDEF HAS_2PARAM_FileAge}
  //single-parameter fileage is deprecated in d2006 and above
  if not FileAge(AFileName, Result) then begin
    Result := 0;
  end;
  {$ELSE}
  LAge := SysUtils.FileAge(AFileName);
  if LAge <> -1 then begin
    Result := FileDateToDateTime(LAge);
  end else begin
    Result := 0.0;
  end;
  {$ENDIF}
end;

function IndyDirectoryExists(const ADirectory: string): Boolean;
{$IFDEF HAS_SysUtils_DirectoryExists}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ELSE}
var
  Code: Integer;
  {$IFDEF STRING_UNICODE_MISMATCH}
  LStr: TIdPlatformString;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF HAS_SysUtils_DirectoryExists}
  Result := SysUtils.DirectoryExists(ADirectory);
  {$ELSE}
  // RLebeau 2/16/2006: Removed dependency on the FileCtrl unit
    {$IFDEF STRING_UNICODE_MISMATCH}
  LStr := TIdPlatformString(ADirectory); // explicit convert to Ansi/Unicode
  Code := GetFileAttributes(PIdPlatformChar(LStr));
    {$ELSE}
  Code := GetFileAttributes(PChar(ADirectory));
    {$ENDIF}
  Result := (Code <> -1) and ((Code and FILE_ATTRIBUTE_DIRECTORY) <> 0);
  {$ENDIF}
end;

function IndyStrToInt64(const S: string; const ADefault: Int64): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := SysUtils.StrToInt64Def(Trim(S), ADefault);
end;

function IndyStrToInt64(const S: string): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := SysUtils.StrToInt64(Trim(S));
end;

function IndyStrToStreamSize(const S: string; const ADefault: TIdStreamSize): TIdStreamSize;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF STREAM_SIZE_64}
  Result := IndyStrToInt64(S, ADefault);
  {$ELSE}
  Result := IndyStrToInt(S, ADefault);
  {$ENDIF}
end;

function IndyStrToStreamSize(const S: string): TIdStreamSize;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF STREAM_SIZE_64}
  Result := IndyStrToInt64(S);
  {$ELSE}
  Result := IndyStrToInt(S);
  {$ENDIF}
end;

function ToBytes(const AValue: string; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ToBytes(AValue, -1, 1, ADestEncoding
    {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
    );
end;

function ToBytes(const AValue: string; const ALength: Integer; const AIndex: Integer = 1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes; overload;
var
  LLength: Integer;
  {$IFDEF STRING_IS_ANSI}
  LBytes: TIdBytes;
  {$ENDIF}
begin
  {$IFDEF STRING_IS_ANSI}
  LBytes := nil; // keep the compiler happy
  {$ENDIF}
  LLength := IndyLength(AValue, ALength, AIndex);
  if LLength > 0 then
  begin
    EnsureEncoding(ADestEncoding);
    {$IFDEF STRING_IS_UNICODE}
    SetLength(Result, ADestEncoding.GetByteCount(AValue, AIndex, LLength));
    if Length(Result) > 0 then begin
      ADestEncoding.GetBytes(AValue, AIndex, LLength, Result, 0);
    end;
    {$ELSE}
    EnsureEncoding(ASrcEncoding, encOSDefault);
    LBytes := RawToBytes(AValue[AIndex], LLength);
    CheckByteEncoding(LBytes, ASrcEncoding, ADestEncoding);
    Result := LBytes;
    {$ENDIF}
  end else begin
    SetLength(Result, 0);
  end;
end;

function ToBytes(const AValue: Char; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  ): TIdBytes; overload;
var
{$IFDEF STRING_IS_UNICODE}
  LChars: {$IFDEF DOTNET}array[0..0] of Char{$ELSE}TIdWideChars{$ENDIF};
{$ELSE}
  LBytes: TIdBytes;
{$ENDIF}
begin
  EnsureEncoding(ADestEncoding);
  {$IFDEF STRING_IS_UNICODE}
    {$IFNDEF DOTNET}
  SetLength(LChars, 1);
    {$ENDIF}
  LChars[0] := AValue;
  Result := ADestEncoding.GetBytes(LChars);
  {$ELSE}
  EnsureEncoding(ASrcEncoding, encOSDefault);
  LBytes := RawToBytes(AValue, 1);
  CheckByteEncoding(LBytes, ASrcEncoding, ADestEncoding);
  Result := LBytes;
  {$ENDIF}
end;

function ToBytes(const AValue: Int64): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.BitConverter.GetBytes(AValue);
  {$ELSE}
  SetLength(Result, SizeOf(Int64));
  PInt64(@Result[0])^ := AValue;
  {$ENDIF}
end;

function ToBytes(const AValue: TIdUInt64): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.BitConverter.GetBytes(AValue);
  {$ELSE}
  SetLength(Result, SizeOf(UInt64));
  PUInt64(@Result[0])^ := AValue{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF};
  {$ENDIF}
end;

function ToBytes(const AValue: Int32): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.BitConverter.GetBytes(AValue);
  {$ELSE}
  SetLength(Result, SizeOf(Int32));
  PInt32(@Result[0])^ := AValue;
  {$ENDIF}
end;

function ToBytes(const AValue: UInt32): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.BitConverter.GetBytes(AValue);
  {$ELSE}
  SetLength(Result, SizeOf(UInt32));
  PUInt32(@Result[0])^ := AValue;
  {$ENDIF}
end;

function ToBytes(const AValue: Int16): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.BitConverter.GetBytes(AValue);
  {$ELSE}
  SetLength(Result, SizeOf(Int16));
  PInt16(@Result[0])^ := AValue;
  {$ENDIF}
end;

function ToBytes(const AValue: UInt16): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.BitConverter.GetBytes(AValue);
  {$ELSE}
  SetLength(Result, SizeOf(UInt16));
  PUInt16(@Result[0])^ := AValue;
  {$ENDIF}
end;

function ToBytes(const AValue: Int8): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  SetLength(Result, SizeOf(Int8));
  Result[0] := Byte(AValue);
end;

function ToBytes(const AValue: UInt8): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  SetLength(Result, SizeOf(UInt8));
  Result[0] := AValue;
end;

function ToBytes(const AValue: TIdBytes; const ASize: Integer; const AIndex: Integer = 0): TIdBytes; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LSize: Integer;
begin
  LSize := IndyLength(AValue, ASize, AIndex);
  SetLength(Result, LSize);
  if LSize > 0 then begin
    CopyTIdBytes(AValue, AIndex, Result, 0, LSize);
  end;
end;

{$IFNDEF DOTNET}
function RawToBytes(const AValue; const ASize: Integer): TIdBytes;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  SetLength(Result, ASize);
  if ASize > 0 then begin
    Move(AValue, Result[0], ASize);
  end;
end;
{$ENDIF}

procedure ToBytesF(var Bytes: TIdBytes; const AValue: Char; ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LChars: {$IFDEF DOTNET}array[0..0] of Char{$ELSE}TIdWideChars{$ENDIF};
begin
  EnsureEncoding(ADestEncoding);
  {$IFDEF STRING_IS_UNICODE}
    {$IFNDEF DOTNET}
  SetLength(LChars, 1);
    {$ENDIF}
  LChars[0] := AValue;
  {$ELSE}
  EnsureEncoding(ASrcEncoding, encOSDefault);
  LChars := ASrcEncoding.GetChars(RawToBytes(AValue, 1));  // convert to Unicode
  {$ENDIF}
  Assert(Length(Bytes) >= ADestEncoding.GetByteCount(LChars));
  ADestEncoding.GetBytes(LChars, 0, Length(LChars), Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int32);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  CopyTIdInt32(AValue, Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int16);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  CopyTIdInt16(AValue, Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: UInt16);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  CopyTIdUInt16(AValue, Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int8);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  Bytes[0] := Byte(AValue);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: UInt8);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  Bytes[0] := AValue;
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: UInt32);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  CopyTIdUInt32(AValue, Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: Int64);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  CopyTIdInt64(AValue, Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: TIdUInt64);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= SizeOf(AValue));
  CopyTIdUInt64(AValue, Bytes, 0);
end;

procedure ToBytesF(var Bytes: TIdBytes; const AValue: TIdBytes; const ASize: Integer; const AIndex: Integer = 0);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= ASize);
  CopyTIdBytes(AValue, AIndex, Bytes, 0, ASize);
end;

{$IFNDEF DOTNET}
procedure RawToBytesF(var Bytes: TIdBytes; const AValue; const ASize: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(Bytes) >= ASize);
  if ASize > 0 then begin
    Move(AValue, Bytes[0], ASize);
  end;
end;
{$ENDIF}

function BytesToChar(const AValue: TIdBytes; const AIndex: Integer = 0;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Char; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  BytesToChar(AValue, Result, AIndex, AByteEncoding{$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF});
end;

function BytesToChar(const AValue: TIdBytes; var VChar: Char; const AIndex: Integer = 0;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Integer; overload;
var
  I, J, NumChars, NumBytes: Integer;
  {$IFDEF DOTNET}
  LChars: array[0..1] of Char;
  {$ELSE}
  LChars: TIdWideChars;
    {$IFDEF STRING_IS_ANSI}
  LWTmp: WideString;
  LATmp: TIdBytes;
    {$ENDIF}
  {$ENDIF}
begin
  Result := 0;
  EnsureEncoding(AByteEncoding);
  // 2 Chars to handle UTF-16 surrogates
  NumBytes := IndyMin(IndyLength(AValue, -1, AIndex), AByteEncoding.GetMaxByteCount(2));
  {$IFNDEF DOTNET}
  SetLength(LChars, 2);
  {$ENDIF}
  NumChars := 0;
  if NumBytes > 0 then
  begin
    for I := 1 to NumBytes do
    begin
      NumChars := AByteEncoding.GetChars(AValue, AIndex, I, LChars, 0);
      Inc(Result);
      if NumChars > 0 then begin
        // RLebeau 10/19/2012: when Indy switched to its own UTF-8 implementation
        // to avoid the MB_ERR_INVALID_CHARS flag on Windows, it accidentally broke
        // this loop!  Since this is not commonly used, this was not noticed until
        // now.  On Windows at least, GetChars() now returns >0 for an invalid
        // sequence, so we have to check if any of the returned characters are the
        // Unicode U+FFFD character, indicating bad data...
        for J := 0 to NumChars-1 do begin
          if LChars[J] = TIdWideChar($FFFD) then begin
            // keep reading...
            NumChars := 0;
            Break;
          end;
        end;
        if NumChars > 0 then begin
          Break;
        end;
      end;
    end;
  end;
  {$IFDEF STRING_IS_UNICODE}
  // RLebeau: if the bytes were decoded into surrogates, the second
  // surrogate is lost here, as it can't be returned unless we cache
  // it somewhere for the the next BytesToChar() call to retreive.  Just
  // raise an error for now.  Users will have to update their code to
  // read surrogates differently...
  Assert(NumChars = 1);
  VChar := LChars[0];
  {$ELSE}
  // RLebeau: since we can only return an AnsiChar here, let's convert
  // the decoded characters, surrogates and all, into their Ansi
  // representation. This will have the same problem as above if the
  // conversion results in a multibyte character sequence...
  EnsureEncoding(ADestEncoding, encOSDefault);
  SetString(LWTmp, PWideChar(LChars), NumChars);
  LATmp := ADestEncoding.GetBytes(LWTmp); // convert to Ansi
  Assert(Length(LATmp) = 1);
  VChar := Char(LATmp[0]);
  {$ENDIF}
end;

function BytesToInt32(const AValue: TIdBytes; const AIndex: Integer = 0): Int32;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+SizeOf(Int32)));
  {$IFDEF DOTNET}
  Result := System.BitConverter.ToInt32(AValue, AIndex);
  {$ELSE}
  Result := PInt32(@AValue[AIndex])^;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
function BytesToLongInt(const AValue: TIdBytes; const AIndex: Integer = 0): Integer;
{$I IdDeprecatedImplBugOff.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := BytesToInt32(AValue, AIndex);
end;

function BytesToInt64(const AValue: TIdBytes; const AIndex: Integer = 0): Int64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+SizeOf(Int64)));
  {$IFDEF DOTNET}
  Result := System.BitConverter.ToInt64(AValue, AIndex);
  {$ELSE}
  Result := PInt64(@AValue[AIndex])^;
  {$ENDIF}
end;

function BytesToUInt64(const AValue: TIdBytes; const AIndex: Integer = 0): TIdUInt64;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+SizeOf(TIdUInt64)));
  {$IFDEF DOTNET}
  Result := System.BitConverter.ToUInt64(AValue, AIndex);
  {$ELSE}
  Result{$IFDEF TIdUInt64_HAS_QuadPart}.QuadPart{$ENDIF} := PUInt64(@AValue[AIndex])^;
  {$ENDIF}
end;

function BytesToTicks(const AValue: TIdBytes; const AIndex: Integer = 0): TIdTicks;
{$IFDEF USE_TIdTicks_TIdUInt64_CONVERSION}
var
  LValue: TIdUInt64;
{$ELSE}
  {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  {$IFDEF USE_TIdTicks_TIdUInt64_CONVERSION}
  // In C++Builder 2006/2007, TIdUInt64 is a packed record, but TIdTicks is
  // an alias for a native UInt64 , so need a conversion here to get around
  // a compiler error: "E2010 Incompatible types: 'UInt64' and 'TIdUInt64'"...
  LValue := BytesToUInt64(AValue, AIndex);
  Result := LValue.QuadPart;
  {$ELSE}
    {$IFDEF UInt64_IS_NATIVE}
  Result := BytesToUInt64(AValue, AIndex);
    {$ELSE}
  Result := BytesToInt64(AValue, AIndex);
    {$ENDIF}
  {$ENDIF}
end;

function BytesToUInt16(const AValue: TIdBytes; const AIndex: Integer = 0): UInt16;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+SizeOf(UInt16)));
  {$IFDEF DOTNET}
  Result := System.BitConverter.ToUInt16(AValue, AIndex);
  {$ELSE}
  Result := PUInt16(@AValue[AIndex])^;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
function BytesToWord(const AValue: TIdBytes; const AIndex: Integer = 0): UInt16;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := BytesToUInt16(AValue, AIndex);
end;

function BytesToInt16(const AValue: TIdBytes; const AIndex: Integer = 0): Int16;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+SizeOf(Int16)));
  {$IFDEF DOTNET}
  Result := System.BitConverter.ToInt16(AValue, AIndex);
  {$ELSE}
  Result := PInt16(@AValue[AIndex])^;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
function BytesToShort(const AValue: TIdBytes; const AIndex: Integer = 0): Int16;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := BytesToInt16(AValue, AIndex);
end;

function BytesToIPv4Str(const AValue: TIdBytes; const AIndex: Integer = 0): String;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+4));
  Result := IntToStr(Ord(AValue[AIndex])) + '.' +
           IntToStr(Ord(AValue[AIndex+1])) + '.' +
           IntToStr(Ord(AValue[AIndex+2])) + '.' +
           IntToStr(Ord(AValue[AIndex+3]));
end;

procedure BytesToIPv6(const AValue: TIdBytes; var VAddress: TIdIPv6Address; const AIndex: Integer = 0);
{$IFDEF DOTNET}
var
  I: Integer;
{$ELSE}
{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+16));
  {$IFDEF DOTNET}
  for i := 0 to 7 do begin
    VAddress[i] := TwoByteToUInt16(AValue[(i*2)+AIndex], AValue[(i*2)+1+AIndex]);
  end;
  {$ELSE}
  Move(AValue[AIndex], VAddress[0], 16);
  {$ENDIF}
end;

function BytesToUInt32(const AValue: TIdBytes; const AIndex: Integer = 0): UInt32;
 {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= (AIndex+SizeOf(UInt32)));
  {$IFDEF DOTNET}
  Result := System.BitConverter.ToUInt32(AValue, AIndex);
  {$ELSE}
  Result := PUInt32(@AValue[AIndex])^;
  {$ENDIF}
end;

{$I IdDeprecatedImplBugOff.inc}
function BytesToLongWord(const AValue: TIdBytes; const AIndex: Integer = 0): UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := BytesToUInt32(AValue, AIndex);
end;

function BytesToString(const AValue: TIdBytes; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := BytesToString(AValue, 0, -1, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

function BytesToString(const AValue: TIdBytes; const AStartIndex: Integer;
  const ALength: Integer = -1; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;
var
  LLength: Integer;
  {$IFDEF STRING_IS_ANSI}
  LBytes: TIdBytes;
  {$ENDIF}
begin
  {$IFDEF STRING_IS_ANSI}
  LBytes := nil; // keep the compiler happy
  {$ENDIF}
  LLength := IndyLength(AValue, ALength, AStartIndex);
  if LLength > 0 then begin
    EnsureEncoding(AByteEncoding);
    {$IFDEF STRING_IS_UNICODE}
    Result := AByteEncoding.GetString(AValue, AStartIndex, LLength);
    {$ELSE}
    EnsureEncoding(ADestEncoding);
    if (AStartIndex = 0) and (LLength = Length(AValue)) then begin
      LBytes := AValue;
    end else begin
      LBytes := Copy(AValue, AStartIndex, LLength);
    end;
    CheckByteEncoding(LBytes, AByteEncoding, ADestEncoding);
    SetString(Result, PAnsiChar(LBytes), Length(LBytes));
    // TODO: on compilers that support AnsiString codepages,
    // set the string's codepage to match ADestEncoding...
    {$ENDIF}
  end else begin
    Result := '';
  end;
end;

function BytesToStringRaw(const AValue: TIdBytes): string; overload;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := BytesToStringRaw(AValue, 0, -1);
end;

function BytesToStringRaw(const AValue: TIdBytes; const AStartIndex: Integer;
  const ALength: Integer = -1): string;
var
  LLength: Integer;
begin
  LLength := IndyLength(AValue, ALength, AStartIndex);
  if LLength > 0 then begin
    {$IFDEF STRING_IS_UNICODE}
    Result := IndyTextEncoding_8Bit.GetString(AValue, AStartIndex, LLength);
    {$ELSE}
    SetString(Result, PAnsiChar(@AValue[AStartIndex]), LLength);
    // TODO: on compilers that support AnsiString codepages,
    // set the string's codepage to something like ISO-8859-1...
    {$ENDIF}
  end else begin
    Result := '';
  end;
end;

{$IFNDEF DOTNET}
procedure BytesToRaw(const AValue: TIdBytes; var VBuffer; const ASize: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Assert(Length(AValue) >= ASize);
  Move(AValue[0], VBuffer, ASize);
end;
{$ENDIF}

function TwoByteToUInt16(AByte1, AByte2: Byte): UInt16;
//Since Replys are returned as Strings, we need a routine to convert two
// characters which are a 2 byte U Int into a two byte unsigned Integer
var
  LWord: TIdBytes;
begin
  SetLength(LWord, SizeOf(UInt16));
  LWord[0] := AByte1;
  LWord[1] := AByte2;
  Result := BytesToUInt16(LWord);
//  Result := UInt16((AByte1 shl 8) and $FF00) or UInt16(AByte2 and $00FF);
end;

{$I IdDeprecatedImplBugOff.inc}
function TwoByteToWord(AByte1, AByte2: Byte): UInt16;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := TwoByteToUInt16(AByte1, AByte2);
end;

function ReadStringFromStream(AStream: TStream; ASize: Integer = -1;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LBytes: TIdBytes;
begin
  ASize := TIdStreamHelper.ReadBytes(AStream, LBytes, ASize);
  Result := BytesToString(LBytes, 0, ASize, AByteEncoding
  {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
  );
end;

function ReadTIdBytesFromStream(const AStream: TStream; var ABytes: TIdBytes;
  const Count: TIdStreamSize; const AIndex: Integer = 0): TIdStreamSize;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := TIdStreamHelper.ReadBytes(AStream, ABytes, Count, AIndex);
end;

function ReadCharFromStream(AStream: TStream; var VChar: Char;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Integer;
var
  StartPos: TIdStreamSize;
  Lb: Byte;
  I, NumChars, NumBytes: Integer;
  LBytes: TIdBytes;
  {$IFDEF DOTNET}
  LChars: array[0..1] of Char;
  {$ELSE}
  LChars: TIdWideChars;
    {$IFDEF STRING_IS_ANSI}
  LWTmp: WideString;
  LATmp: TIdBytes;
    {$ENDIF}
  {$ENDIF}

  function ReadByte: Byte;
  begin
    if AStream.Read(Result{$IFNDEF DOTNET}, 1{$ENDIF}) <> 1 then begin
      raise EIdException.Create('Unable to read byte'); {do not localize}
    end;
  end;

begin
  Result := 0;
  {$IFDEF STRING_IS_ANSI}
  LATmp := nil; // keep the compiler happy
  {$ENDIF}

  EnsureEncoding(AByteEncoding);
  StartPos := AStream.Position;

  // don't raise an exception here, backwards compatibility for now
  if AStream.Read(Lb{$IFNDEF DOTNET}, 1{$ENDIF}) <> 1 then begin
    Exit;
  end;
  Result := 1;

  // 2 Chars to handle UTF-16 surrogates
  NumBytes := AByteEncoding.GetMaxByteCount(2);
  SetLength(LBytes, NumBytes);
  {$IFNDEF DOTNET}
  SetLength(LChars, 2);
  {$ENDIF}

  try
    repeat
      LBytes[Result-1] := Lb;
      NumChars := AByteEncoding.GetChars(LBytes, 0, Result, LChars, 0);
      if NumChars > 0 then begin
        // RLebeau 10/19/2012: when Indy switched to its own UTF-8 implementation
        // to avoid the MB_ERR_INVALID_CHARS flag on Windows, it accidentally broke
        // this loop!  Since this is not commonly used, this was not noticed until
        // now.  On Windows at least, GetChars() now returns >0 for an invalid
        // sequence, so we have to check if any of the returned characters are the
        // Unicode U+FFFD character, indicating bad data...
        for I := 0 to NumChars-1 do begin
          if LChars[I] = TIdWideChar($FFFD) then begin
            // keep reading...
            NumChars := 0;
            Break;
          end;
        end;
        if NumChars > 0 then begin
          Break;
        end;
      end;
      if Result = NumBytes then begin
        Break;
      end;
      Lb := ReadByte;
      Inc(Result);
    until False;
  except
    AStream.Position := StartPos;
    raise;
  end;

  {$IFDEF STRING_IS_UNICODE}
  // RLebeau: if the bytes were decoded into surrogates, the second
  // surrogate is lost here, as it can't be returned unless we cache
  // it somewhere for the the next ReadTIdBytesFromStream() call to
  // retreive.  Just raise an error for now.  Users will have to
  // update their code to read surrogates differently...
  Assert(NumChars = 1);
  VChar := LChars[0];
  {$ELSE}
  // RLebeau: since we can only return an AnsiChar here, let's convert
  // the decoded characters, surrogates and all, into their Ansi
  // representation. This will have the same problem as above if the
  // conversion results in a multibyte character sequence...
  EnsureEncoding(ADestEncoding, encOSDefault);
  SetString(LWTmp, PWideChar(LChars), NumChars);
  LATmp := ADestEncoding.GetBytes(LWTmp); // convert to Ansi
  Assert(Length(LATmp) = 1);
  VChar := Char(LATmp[0]);
  {$ENDIF}
end;

procedure WriteTIdBytesToStream(const AStream: TStream; const ABytes: TIdBytes;
  const ASize: Integer = -1; const AIndex: Integer = 0);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  TIdStreamHelper.Write(AStream, ABytes, ASize, AIndex);
end;

procedure WriteStringToStream(AStream: TStream; const AStr: string;
  ADestEncoding: IIdTextEncoding
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  WriteStringToStream(AStream, AStr, -1, 1, ADestEncoding
    {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
    );
end;

procedure WriteStringToStream(AStream: TStream; const AStr: string;
  const ALength: Integer = -1; const AIndex: Integer = 1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
{$IFDEF USE_INLINE}inline;{$ENDIF}
var
  LLength: Integer;
  LBytes: TIdBytes;
begin
  LBytes := nil;
  LLength := IndyLength(AStr, ALength, AIndex);
  if LLength > 0 then
  begin
    LBytes := ToBytes(AStr, LLength, AIndex, ADestEncoding
    {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
    );
    TIdStreamHelper.Write(AStream, LBytes);
  end;
end;

{$IFDEF DOTNET}
function TIdBaseStream.Read(var VBuffer: array of Byte; AOffset, ACount: Longint): Longint;
var
  LBytes: TIdBytes;
begin
  // this is a silly work around really, but array of Byte and TIdByte aren't
  // interchangable in a var parameter, though really they *should be*
  SetLength(LBytes, ACount - AOffset);
  Result := IdRead(LBytes, 0, ACount - AOffset);
  CopyTIdByteArray(LBytes, 0, VBuffer, AOffset, Result);
end;

function TIdBaseStream.Write(const ABuffer: array of Byte; AOffset, ACount: Longint): Longint;
begin
  Result := IdWrite(ABuffer, AOffset, ACount);
end;

function TIdBaseStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Result := IdSeek(AOffset, AOrigin);
end;

procedure TIdBaseStream.SetSize(ASize: Int64);
begin
  IdSetSize(ASize);
end;

{$ELSE}

  {$IFDEF STREAM_SIZE_64}
procedure TIdBaseStream.SetSize(const NewSize: Int64);
begin
   IdSetSize(NewSize);
end;
  {$ELSE}
procedure TIdBaseStream.SetSize(ASize: Integer);
begin
  IdSetSize(ASize);
end;
 {$ENDIF}

function TIdBaseStream.Read(var Buffer; Count: Longint): Longint;
var
  LBytes: TIdBytes;
begin
  SetLength(LBytes, Count);
  Result := IdRead(LBytes, 0, Count);
  if Result > 0 then begin
    Move(LBytes[0], Buffer, Result);
  end;
end;

function TIdBaseStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count > 0 then begin
    Result := IdWrite(RawToBytes(Buffer, Count), 0, Count);
  end else begin
    Result := 0;
  end;
end;

  {$IFDEF STREAM_SIZE_64}
function TIdBaseStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := IdSeek(Offset, Origin);
end;
  {$ELSE}
function TIdBaseStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  LSeek : TSeekOrigin;
begin
  case Origin of
    soFromBeginning : LSeek := soBeginning;
    soFromCurrent : LSeek := soCurrent;
    soFromEnd : LSeek := soEnd;
  else
    Result := 0;
    Exit;
  end;
  Result := IdSeek(Offset, LSeek) and $FFFFFFFF;
end;
  {$ENDIF}

{$ENDIF}

function TIdCalculateSizeStream.IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint;
begin
  Result := 0;
end;

function TIdCalculateSizeStream.IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint;
var
  I: Integer;
begin
  I := IndyLength(ABuffer, ACount, AOffset);
  if I > 0 then begin
    Inc(FPosition, I);
    if FPosition > FSize then begin
      FSize := FPosition;
    end;
  end;
  Result := I;
end;

function TIdCalculateSizeStream.IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  case AOrigin of
    soBeginning: begin
      FPosition := AOffset;
    end;
    soCurrent: begin
      FPosition := FPosition + AOffset;
    end;
    soEnd: begin
      FPosition := FSize + AOffset;
    end;
  end;
  if FPosition < 0 then begin
    FPosition := 0;
  end;
  Result := FPosition;
end;

procedure TIdCalculateSizeStream.IdSetSize(ASize: Int64);
begin
  if ASize < 0 then begin
    ASize := 0;
  end;
  if FSize <> ASize then begin
    FSize := ASize;
    if FSize < FPosition then begin
      FPosition := FSize;
    end;
  end;
end;

function TIdEventStream.IdRead(var VBuffer: TIdBytes; AOffset, ACount: Longint): Longint;
begin
  Result := 0;
  if Assigned(FOnRead) then begin
    FOnRead(VBuffer, AOffset, ACount, Result);
  end;
end;

function TIdEventStream.IdWrite(const ABuffer: TIdBytes; AOffset, ACount: Longint): Longint;
begin
  if Assigned(FOnWrite) then begin
    Result := 0;
    FOnWrite(ABuffer, AOffset, ACount, Result);
  end else begin
    Result := ACount;
  end;
end;

function TIdEventStream.IdSeek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Result := 0;
  if Assigned(FOnSeek) then begin
    FOnSeek(AOffset, AOrigin, Result);
  end;
end;

procedure TIdEventStream.IdSetSize(ASize: Int64);
begin
  if Assigned(FOnSetSize) then begin
    FOnSetSize(ASize);
  end;
end;

{$IFNDEF DOTNET}
constructor TIdMemoryBufferStream.Create(APtr: Pointer; ASize: TIdNativeInt);
begin
  inherited Create;
  SetPointer(APtr, ASize);
end;

{$UNDEF USE_PBYTE_ARITHMETIC}
{$IFDEF FPC}
  {$DEFINE USE_PBYTE_ARITHMETIC}
{$ELSE}
  {$IFDEF VCL_XE2_OR_ABOVE}
    {$DEFINE USE_PBYTE_ARITHMETIC}
  {$ENDIF}
{$ENDIF}

function TIdMemoryBufferStream.Write(const Buffer; Count: Longint): Longint;
var
  LAvailable: TIdStreamSize;
  LNumToCopy: Longint;
begin
  Result := 0;
  LAvailable := Size - Position;
  if LAvailable > 0 then
  begin
    {$IFDEF STREAM_SIZE_64}
    LNumToCopy := Longint(IndyMin(LAvailable, TIdStreamSize(Count)));
    {$ELSE}
    LNumToCopy := IndyMin(LAvailable, Count);
    {$ENDIF}
    if LNumToCopy > 0 then
    begin
      System.Move(Buffer, ({$IFDEF USE_PBYTE_ARITHMETIC}PByte{$ELSE}PIdAnsiChar{$ENDIF}(Memory) + Position)^, LNumToCopy);
      TIdStreamHelper.Seek(Self, LNumToCopy, soCurrent);
      Result := LNumToCopy;
    end;
  end;
end;
{$ENDIF}

procedure AppendBytes(var VBytes: TIdBytes; const AToAdd: TIdBytes; const AIndex: Integer = 0; const ALength: Integer = -1);
var
  LOldLen, LAddLen: Integer;
begin
  LAddLen := IndyLength(AToAdd, ALength, AIndex);
  if LAddLen > 0 then begin
    LOldLen := Length(VBytes);
    SetLength(VBytes, LOldLen + LAddLen);
    CopyTIdBytes(AToAdd, AIndex, VBytes, LOldLen, LAddLen);
  end;
end;

procedure AppendByte(var VBytes: TIdBytes; const AByte: Byte);
var
  LOldLen: Integer;
begin
  LOldLen := Length(VBytes);
  SetLength(VBytes, LOldLen + 1);
  VBytes[LOldLen] := AByte;
end;

procedure AppendString(var VBytes: TIdBytes; const AStr: String; const ALength: Integer = -1;
  ADestEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  LBytes: TIdBytes;
  LLength, LOldLen: Integer;
begin
  LBytes := nil; // keep the compiler happy
  LLength := IndyLength(AStr, ALength);
  if LLength > 0 then begin
    LBytes := ToBytes(AStr, LLength, 1, ADestEncoding
      {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
      );
    LOldLen := Length(VBytes);
    LLength := Length(LBytes);
    SetLength(VBytes, LOldLen + LLength);
    CopyTIdBytes(LBytes, 0, VBytes, LOldLen, LLength);
  end;
end;

procedure ExpandBytes(var VBytes: TIdBytes; const AIndex: Integer; const ACount: Integer; const AFillByte: Byte = 0);
var
  I: Integer;
begin
  if ACount > 0 then begin
    // if AIndex is at the end of the buffer then the operation is appending bytes
    if AIndex <> Length(VBytes) then begin
      //if these asserts fail, then it indicates an attempted buffer overrun.
      Assert(AIndex >= 0);
      Assert(AIndex < Length(VBytes));
    end;
    SetLength(VBytes, Length(VBytes) + ACount);
    // move any existing bytes at the index to the end of the buffer
    for I := Length(VBytes)-1 downto AIndex+ACount do begin
      VBytes[I] := VBytes[I-ACount];
    end;
    // fill in the new space with the fill byte
    for I := AIndex to AIndex+ACount-1 do begin
      VBytes[I] := AFillByte;
    end;
  end;
end;

procedure InsertBytes(var VBytes: TIdBytes; const ADestIndex: Integer;
  const ASource: TIdBytes; const ASourceIndex: Integer = 0);
var
  LAddLen: Integer;
begin
  LAddLen := IndyLength(ASource, -1, ASourceIndex);
  if LAddLen > 0 then begin
    ExpandBytes(VBytes, ADestIndex, LAddLen);
    CopyTIdBytes(ASource, ASourceIndex, VBytes, ADestIndex, LAddLen);
  end;
end;

procedure InsertByte(var VBytes: TIdBytes; const AByte: Byte; const AIndex: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  ExpandBytes(VBytes, AIndex, 1, AByte);
end;

procedure RemoveBytes(var VBytes: TIdBytes; const ACount: Integer; const AIndex: Integer = 0);
var
  I: Integer;
  LActual: Integer;
begin
  //TODO: check the reference count of VBytes, if >1 then make a new copy
  Assert(AIndex >= 0);
  LActual := IndyMin(Length(VBytes)-AIndex, ACount);
  if LActual > 0 then begin
    if (AIndex + LActual) < Length(VBytes) then begin
      // RLebeau: TODO - use Move() here instead?
      for I := AIndex to Length(VBytes)-LActual-1 do begin
        VBytes[I] := VBytes[I+LActual];
      end;
    end;
    SetLength(VBytes, Length(VBytes)-LActual);
  end;
end;

procedure IdDelete(var s: string; AOffset, ACount: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Delete(s, AOffset, ACount);
end;

procedure IdInsert(const Source: string; var S: string; Index: Integer);
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Insert(Source, S, Index);
end;

function TextIsSame(const A1, A2: string): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := System.String.Compare(A1, A2, True) = 0;
  {$ELSE}
  Result := AnsiCompareText(A1, A2) = 0;
  {$ENDIF}
end;

// TODO: define STRING_UNICODE_MISMATCH for WinCE in IdCompilerDefines.inc?
{$IFDEF WINDOWS}
  {$IFDEF WINCE}
    {$IFNDEF STRING_IS_UNICODE}
      {$DEFINE COMPARE_STRING_MISMATCH}
    {$ENDIF}
  {$ELSE}
    {$IFDEF STRING_UNICODE_MISMATCH}
      {$DEFINE COMPARE_STRING_MISMATCH}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

function TextStartsWith(const S, SubS: string): Boolean;
var
  LLen: Integer;
  {$IFDEF WINDOWS}
    {$IFDEF COMPARE_STRING_MISMATCH}
  LS, LSubS: {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF};
  P1, P2: {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF};
    {$ENDIF}
  {$ENDIF}
begin
  LLen := Length(SubS);
  Result := LLen <= Length(S);
  if Result then
  begin
    {$IFDEF DOTNET}
    Result := System.String.Compare(S, 0, SubS, 0, LLen, True) = 0;
    {$ELSE}
      {$IFDEF WINDOWS}
        {$IFDEF COMPARE_STRING_MISMATCH}
    // explicit convert to Ansi/Unicode
    LS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(S);
    LSubS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(SubS);
    LLen := Length(LSubS);
    Result := LLen <= Length(LS);
    if Result then begin
      P1 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LS);
      P2 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LSubS);
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, P1, LLen, P2, LLen) = 2;
    end;
        {$ELSE}
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(S), LLen, PChar(SubS), LLen) = 2;
        {$ENDIF}
      {$ELSE}
    Result := AnsiCompareText(Copy(S, 1, LLen), SubS) = 0;
      {$ENDIF}
    {$ENDIF}
  end;
end;

function TextEndsWith(const S, SubS: string): Boolean;
var
  LLen: Integer;
  {$IFDEF WINDOWS}
    {$IFDEF COMPARE_STRING_MISMATCH}
  LS, LSubS: {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF};
  P1, P2: {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF};
    {$ELSE}
  P: PChar;
    {$ENDIF}
  {$ENDIF}
begin
  LLen := Length(SubS);
  Result := LLen <= Length(S);
  if Result then
  begin
    {$IFDEF DOTNET}
    Result := System.String.Compare(S, Length(S)-LLen, SubS, 0, LLen, True) = 0;
    {$ELSE}
      {$IFDEF WINDOWS}
        {$IFDEF COMPARE_STRING_MISMATCH}
    // explicit convert to Ansi/Unicode
    LS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(S);
    LSubS := {$IFDEF WINCE}TIdUnicodeString{$ELSE}TIdPlatformString{$ENDIF}(SubS);
    LLen := Length(LSubS);
    Result := LLen <= Length(S);
    if Result then begin
      P1 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LS);
      P2 := {$IFDEF WINCE}PIdWideChar{$ELSE}PIdPlatformChar{$ENDIF}(LSubS);
      Inc(P1, Length(LS)-LLen);
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, P1, LLen, P2, LLen) = 2;
    end;
        {$ELSE}
    P := PChar(S);
    Inc(P, Length(S)-LLen);
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, P, LLen, PChar(SubS), LLen) = 2;
        {$ENDIF}
      {$ELSE}
    Result := AnsiCompareText(Copy(S, Length(S)-LLen+1, LLen), SubS) = 0;
      {$ENDIF}
    {$ENDIF}
  end;
end;

function IndyLowerCase(const A1: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := A1.ToLower;
  {$ELSE}
  Result := AnsiLowerCase(A1);
  {$ENDIF}
end;

function IndyUpperCase(const A1: string): string;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := A1.ToUpper;
  {$ELSE}
  Result := AnsiUpperCase(A1);
  {$ENDIF}
end;

function IndyCompareStr(const A1, A2: string): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF DOTNET}
  Result := CompareStr(A1, A2);
  {$ELSE}
  Result := AnsiCompareStr(A1, A2);
  {$ENDIF}
end;

function CharPosInSet(const AString: string; const ACharPos: Integer; const ASet: String): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFNDEF DOTNET}
var
  LChar: Char;
  I: Integer;
{$ENDIF}
begin
  Result := 0;
  if ACharPos < 1 then begin
    raise EIdException.Create('Invalid ACharPos');{ do not localize }
  end;
  if ACharPos <= Length(AString) then begin
    {$IFDEF DOTNET}
    Result := ASet.IndexOf(AString[ACharPos]) + 1;
    {$ELSE}
    // RLebeau 5/8/08: Calling Pos() with a Char as input creates a temporary
    // String.  Normally this is fine, but profiling reveils this to be a big
    // bottleneck for code that makes a lot of calls to CharIsInSet(), so we
    // will scan through ASet looking for the character without a conversion...
    //
    // Result := IndyPos(AString[ACharPos], ASet);
    //
    LChar := AString[ACharPos];
    for I := 1 to Length(ASet) do begin
      if ASet[I] = LChar then begin
        Result := I;
        Exit;
      end;
    end;
    {$ENDIF}
  end;
end;

function CharIsInSet(const AString: string; const ACharPos: Integer; const ASet:  String): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := CharPosInSet(AString, ACharPos, ASet) > 0;
end;

function CharIsInEOL(const AString: string; const ACharPos: Integer): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := CharPosInSet(AString, ACharPos, EOL) > 0;
end;

function CharEquals(const AString: string; const ACharPos: Integer; const AValue: Char): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ACharPos < 1 then begin
    raise EIdException.Create('Invalid ACharPos');{ do not localize }
  end;
  Result := ACharPos <= Length(AString);
  if Result then begin
    Result := AString[ACharPos] = AValue;
  end;
end;

{$IFDEF STRING_IS_IMMUTABLE}

{$IFDEF DOTNET}
  {$DEFINE HAS_String_IndexOf}
{$ENDIF}
{$IFDEF HAS_SysUtils_TStringHelper}
  {$DEFINE HAS_String_IndexOf}
{$ENDIF}

function CharPosInSet(const ASB: TIdStringBuilder; const ACharPos: Integer; const ASet: String): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFNDEF HAS_String_IndexOf}
var
  LChar: Char;
  I: Integer;
{$ENDIF}
begin
  Result := 0;
  if ACharPos < 1 then begin
    raise EIdException.Create('Invalid ACharPos');{ do not localize }
  end;
  if ACharPos <= ASB.Length then begin
    {$IFDEF HAS_String_IndexOf}
    Result := ASet.IndexOf(ASB[ACharPos-1]) + 1;
    {$ELSE}
    // RLebeau 5/8/08: Calling Pos() with a Char as input creates a temporary
    // String.  Normally this is fine, but profiling reveils this to be a big
    // bottleneck for code that makes a lot of calls to CharIsInSet(), so we
    // will scan through ASet looking for the character without a conversion...
    //
    // Result := IndyPos(ASB[ACharPos-1], ASet);
    //
    LChar := ASB[ACharPos-1];
    for I := 1 to Length(ASet) do begin
      if ASet[I] = LChar then begin
        Result := I;
        Exit;
      end;
    end;
    {$ENDIF}
  end;
end;

function CharIsInSet(const ASB: TIdStringBuilder; const ACharPos: Integer; const ASet:  String): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := CharPosInSet(ASB, ACharPos, ASet) > 0;
end;

function CharIsInEOL(const ASB: TIdStringBuilder; const ACharPos: Integer): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := CharPosInSet(ASB, ACharPos, EOL) > 0;
end;

function CharEquals(const ASB: TIdStringBuilder; const ACharPos: Integer; const AValue: Char): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if ACharPos < 1 then begin
    raise EIdException.Create('Invalid ACharPos');{ do not localize }
  end;
  Result := ACharPos <= ASB.Length;
  if Result then begin
    Result := ASB[ACharPos-1] = AValue;
  end;
end;

{$ENDIF}

function ByteIndex(const AByte: Byte; const ABytes: TIdBytes; const AStartIndex: Integer = 0): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartIndex to Length(ABytes)-1 do begin
    if ABytes[I] = AByte then begin
      Result := I;
      Exit;
    end;
  end;
end;

function ByteIdxInSet(const ABytes: TIdBytes; const AIndex: Integer; const ASet: TIdBytes): Integer;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  if AIndex < 0 then begin
    raise EIdException.Create('Invalid AIndex'); {do not localize}
  end;
  if AIndex < Length(ABytes) then begin
    Result := ByteIndex(ABytes[AIndex], ASet);
  end else begin
    Result := -1;
  end;
end;

function ByteIsInSet(const ABytes: TIdBytes; const AIndex: Integer; const ASet: TIdBytes): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ByteIdxInSet(ABytes, AIndex, ASet) > -1;
end;

function ByteIsInEOL(const ABytes: TIdBytes; const AIndex: Integer): Boolean;
var
  LSet: TIdBytes;
begin
  SetLength(LSet, 2);
  LSet[0] := 13;
  LSet[1] := 10;
  Result := ByteIsInSet(ABytes, AIndex, LSet);
end;

function ReadLnFromStream(AStream: TStream; AMaxLineLength: Integer = -1;
  AExceptionIfEOF: Boolean = False; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string; overload;
begin
  if (not ReadLnFromStream(AStream, Result, AMaxLineLength, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    )) and AExceptionIfEOF then
begin
    raise EIdEndOfStream.CreateFmt(RSEndOfStream, ['ReadLnFromStream', AStream.Position]);
  end;
end;

//TODO: Continue to optimize this function. Its performance severely impacts the coders
function ReadLnFromStream(AStream: TStream; var VLine: String; AMaxLineLength: Integer = -1;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Boolean; overload;
const
  LBUFMAXSIZE = 2048;
var
  LStringLen, LResultLen, LBufSize: Integer;
  LBuf: TIdBytes;
  LLine: TIdBytes;
  // LBuf: packed array [0..LBUFMAXSIZE] of Char;
  LStrmPos, LStrmSize: TIdStreamSize; //LBytesToRead = stream size - Position
  LCrEncountered: Boolean;

  function FindEOL(const ABuf: TIdBytes; var VLineBufSize: Integer; var VCrEncountered: Boolean): Integer;
  var
    i: Integer;
  begin
    Result := VLineBufSize; //EOL not found => use all
    i := 0;
    while i < VLineBufSize do begin
      case ABuf[i] of
        Ord(LF): begin
            Result := i; {string size}
            VCrEncountered := True;
            VLineBufSize := i+1;
            Break;
          end;
        Ord(CR): begin
            Result := i; {string size}
            VCrEncountered := True;
            Inc(i); //crLF?
            if (i < VLineBufSize) and (ABuf[i] = Ord(LF)) then begin
              VLineBufSize := i+1;
            end else begin
              VLineBufSize := i;
            end;
            Break;
          end;
      end;
      Inc(i);
    end;
  end;

begin
  Assert(AStream<>nil);
  VLine := '';
  SetLength(LLine, 0);

  if AMaxLineLength < 0 then begin
    AMaxLineLength := MaxInt;
  end;

  { we store the stream size for the whole routine to prevent
  so do not incur a performance penalty with TStream.Size.  It has
  to use something such as Seek each time the size is obtained}
  {4 seek vs 3 seek}
  LStrmPos := AStream.Position;
  LStrmSize := AStream.Size;

  if LStrmPos >= LStrmSize then begin
    Result := False;
    Exit;
  end;

  SetLength(LBuf, LBUFMAXSIZE);
  LCrEncountered := False;

  repeat
    LBufSize := ReadTIdBytesFromStream(AStream, LBuf, IndyMin(LStrmSize - LStrmPos, LBUFMAXSIZE));
    if LBufSize < 1 then begin
      Break; // TODO: throw a stream read exception instead?
    end;

    LStringLen := FindEOL(LBuf, LBufSize, LCrEncountered);
    Inc(LStrmPos, LBufSize);

    LResultLen := Length(VLine);
    if (LResultLen + LStringLen) > AMaxLineLength then begin
      LStringLen := AMaxLineLength - LResultLen;
      LCrEncountered := True;
      Dec(LStrmPos, LBufSize);
      Inc(LStrmPos, LStringLen);
    end;
    if LStringLen > 0 then begin
      LBufSize := Length(LLine);
      SetLength(LLine, LBufSize+LStringLen);
      CopyTIdBytes(LBuf, 0, LLine, LBufSize, LStringLen);
    end;
  until (LStrmPos >= LStrmSize) or LCrEncountered;

  // RLebeau: why is the original Position being restored here, instead
  // of leaving the Position at the end of the line?
  AStream.Position := LStrmPos;
  VLine := BytesToString(LLine, 0, -1, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
  Result := True;
end;

{$IFNDEF DOTNET}
  {$IFDEF REGISTER_EXPECTED_MEMORY_LEAK}
function IndyRegisterExpectedMemoryLeak(AAddress: Pointer): Boolean;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  // use only System.RegisterExpectedMemoryLeak() on systems that support
  // it. We should use whatever the RTL's active memory manager is.  Fallback
  // to specific memory managers only if System.RegisterExpectedMemoryLeak()
  // is not available.

  {$IFDEF HAS_System_RegisterExpectedMemoryLeak}
  // RLebeau 4/21/08: not quite sure what the difference is between the
  // SysRegisterExpectedMemoryLeak() and RegisterExpectedMemoryLeak()
  // functions in the System unit, but calling RegisterExpectedMemoryLeak()
  // is causing stack overflows when FastMM is not active, so call
  // SysRegisterExpectedMemoryLeak() instead...

  // RLebeau 7/4/09: According to Pierre Le Riche, developer of FastMM:
  //
  // "SysRegisterExpectedMemoryLeak() is the leak registration routine for
  // the built-in memory manager. FastMM.RegisterExpectedMemoryLeak is the
  // leak registration code for FastMM. Both of these are thus hardwired to
  // a specific memory manager. In order to register a leak for the
  // *currently installed* memory manager, which is what you typically want
  // to do, you have to call System.RegisterExpectedMemoryLeak().
  // System.RegisterExpectedMemoryLeak() redirects to the leak registration
  // code of the installed memory manager."

  //Result := System.SysRegisterExpectedMemoryLeak(AAddress);
  Result := System.RegisterExpectedMemoryLeak(AAddress);
  {$ELSE}
    // RLebeau 10/5/2014: the user can override the RTL's version of FastMM
    // (2006+ only) with any memory manager, such as MadExcept, so check for
    // that...
    {$IFDEF USE_FASTMM4}
  Result := FastMM4.RegisterExpectedMemoryLeak(AAddress);
    {$ELSE}
      {$IFDEF USE_MADEXCEPT}
  Result := madExcept.HideLeak(AAddress);
      {$ELSE}
  Result := False;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;
  {$ENDIF}
{$ENDIF}

function IndyAddPair(AStrings: TStrings; const AName, AValue: String): TStrings;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_TStrings_AddPair}
  Result := AStrings.AddPair(AName, AValue);
  {$ELSE}
    {$IFDEF HAS_TStrings_NameValueSeparator}
  AStrings.Add(AName + AStrings.NameValueSeparator + AValue);
    {$ELSE}
  AStrings.Add(AName + '=' + AValue); {do not localize}
    {$ENDIF}
  Result := AStrings;
  {$ENDIF}
end;

function IndyAddPair(AStrings: TStrings; const AName, AValue: String; AObject: TObject): TStrings;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_TStrings_AddPair}
  Result := AStrings.AddPair(AName, AValue, AObject);
  {$ELSE}
    {$IFDEF HAS_TStrings_NameValueSeparator}
  AStrings.AddObject(AName + AStrings.NameValueSeparator + AValue, AObject);
    {$ELSE}
  AStrings.AddObject(AName + '=' + AValue, AObject);
    {$ENDIF}
  Result := AStrings;
  {$ENDIF}
end;

function InternalIndyIndexOf(AStrings: TStrings; const AStr: string;
  const ACaseSensitive: Boolean = False): Integer;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AStrings.Count - 1 do begin
    if ACaseSensitive then begin
      if AStrings[I] = AStr then begin
        Result := I;
        Exit;
      end;
    end else begin
      if TextIsSame(AStrings[I], AStr) then begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

function IndyIndexOf(AStrings: TStrings; const AStr: string;
  const ACaseSensitive: Boolean = False): Integer;
begin
  {$IFDEF HAS_TStringList_CaseSensitive}
  if AStrings is TStringList then begin
    Result := IndyIndexOf(TStringList(AStrings), AStr, ACaseSensitive);
    Exit;
  end;
  {$ENDIF}
  Result := InternalIndyIndexOf(AStrings, AStr, ACaseSensitive);
end;

{$IFDEF HAS_TStringList_CaseSensitive}
function IndyIndexOf(AStrings: TStringList; const AStr: string;
  const ACaseSensitive: Boolean = False): Integer;
begin
  if AStrings.CaseSensitive = ACaseSensitive then begin
    Result := AStrings.IndexOf(AStr);
  end else begin
    Result := InternalIndyIndexOf(AStrings, AStr, ACaseSensitive);
  end;
end;
{$ENDIF}

function InternalIndyIndexOfName(AStrings: TStrings; const AName: string;
  const ACaseSensitive: Boolean = False): Integer;
  {$IFDEF USE_INLINE}inline;{$ENDIF}
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AStrings.Count - 1 do begin
    if ACaseSensitive then begin
      if AStrings.Names[I] = AName then begin
        Result := I;
        Exit;
      end;
    end
    else if TextIsSame(AStrings.Names[I], AName) then begin
      Result := I;
      Exit;
    end;
  end;
end;

function IndyIndexOfName(AStrings: TStrings; const AName: string;
  const ACaseSensitive: Boolean = False): Integer;
begin
  {$IFDEF HAS_TStringList_CaseSensitive}
  if AStrings is TStringList then begin
    Result := IndyIndexOfName(TStringList(AStrings), AName, ACaseSensitive);
    Exit;
  end;
  {$ENDIF}
  Result := InternalIndyIndexOfName(AStrings, AName, ACaseSensitive);
end;

{$IFDEF HAS_TStringList_CaseSensitive}
function IndyIndexOfName(AStrings: TStringList; const AName: string;
  const ACaseSensitive: Boolean = False): Integer;
begin
  if AStrings.CaseSensitive = ACaseSensitive then begin
    Result := AStrings.IndexOfName(AName);
  end else begin
    Result := InternalIndyIndexOfName(AStrings, AName, ACaseSensitive);
  end;
end;
{$ENDIF}

function IndyValueFromIndex(AStrings: TStrings; const AIndex: Integer): String;
{$IFNDEF HAS_TStrings_ValueFromIndex}
var
  LTmp: string;
  LPos: Integer;
  {$IFDEF HAS_TStrings_NameValueSeparator}
  LChar: Char;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF HAS_TStrings_ValueFromIndex}
  Result := AStrings.ValueFromIndex[AIndex];
  {$ELSE}
  Result := '';
  if AIndex >= 0 then
  begin
    LTmp := AStrings.Strings[AIndex];
    {$IFDEF HAS_TStrings_NameValueSeparator}
    // RLebeau 11/8/16: Calling Pos() with a Char as input creates a temporary
    // String.  Normally this is fine, but profiling reveils this to be a big
    // bottleneck for code that makes a lot of calls to Pos() in a loop, so we
    // will scan through the string looking for the character without a conversion...
    //
    // LPos := Pos(AStrings.NameValueSeparator, LTmp); {do not localize}
    // if LPos > 0 then begin
    //
    LChar := AStrings.NameValueSeparator;
    for LPos := 1 to Length(LTmp) do begin
      //if CharEquals(LTmp, LPos, LChar) then begin
      if LTmp[LPos] = LChar then begin
        Result := Copy(LTmp, LPos+1, MaxInt);
        Exit;
      end;
    end;
    {$ELSE}
    LPos := Pos('=', LTmp); {do not localize}
    if LPos > 0 then begin
      Result := Copy(LTmp, LPos+1, MaxInt);
    end;
    {$ENDIF}
  end;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
function IndyWindowsMajorVersion: Integer;
begin
  {$IFDEF WINCE}
  Result := SysUtils.WinCEMajorVersion;
  {$ELSE}
  Result := SysUtils.Win32MajorVersion;
  {$ENDIF}
end;

function IndyWindowsMinorVersion: Integer;
begin
  {$IFDEF WINCE}
  Result := SysUtils.WinCEMinorVersion;
  {$ELSE}
  Result := SysUtils.Win32MinorVersion;
  {$ENDIF}
end;

function IndyWindowsBuildNumber: Integer;
begin
  // for this, you need to strip off some junk to do comparisons
  {$IFDEF WINCE}
  Result := SysUtils.WinCEBuildNumber and $FFFF;
  {$ELSE}
  Result := SysUtils.Win32BuildNumber and $FFFF;
  {$ENDIF}
end;

function IndyWindowsPlatform: Integer;
begin
  {$IFDEF WINCE}
  Result := SysUtils.WinCEPlatform;
  {$ELSE}
  Result := SysUtils.Win32Platform;
  {$ENDIF}
end;

function IndyCheckWindowsVersion(const AMajor: Integer; const AMinor: Integer = 0): Boolean;
var
  LMajor, LMinor: Integer;
begin
  LMajor := IndyWindowsMajorVersion;
  LMinor := IndyWindowsMinorVersion;
  Result := (LMajor > AMajor) or ((LMajor = AMajor) and (LMinor >= AMinor));
end;
{$ENDIF}

procedure IdDisposeAndNil(var Obj);
{$IFDEF USE_OBJECT_ARC}
var
  Temp: {Pointer}TObject;
{$ENDIF}
begin
  {$IFDEF USE_OBJECT_ARC}
  // RLebeau: was originally calling DisposeOf() on Obj directly, but nil'ing
  // Obj first prevented the calling code from invoking __ObjRelease() on Obj.
  // Don't do that in ARC.  __ObjRelease() needs to be called, even if disposed,
  // to allow the compiler/RTL to finalize Obj so any managed members it has
  // can be cleaned up properly...
  {
  Temp := Pointer(Obj);
  Pointer(Obj) := nil;
  TObject(Temp).DisposeOf;
  }
  Pointer(Temp) := Pointer(Obj);
  Pointer(Obj) := nil;
  Temp.DisposeOf;
  // __ObjRelease() is called when Temp goes out of scope
  {$ELSE}
  FreeAndNil(Obj);
  {$ENDIF}
end;

initialization
  // AnsiPos does not handle strings with #0 and is also very slow compared to Pos
  {$IFDEF DOTNET}
  IndyPos := SBPos;
  {$ELSE}
  if LeadBytes = [] then begin
    IndyPos := SBPos;
  end else begin
    IndyPos := InternalAnsiPos;
  end;
  {$ENDIF}
  {$IFDEF DYNAMICLOAD_InterlockedCompareExchange}
  InterlockedCompareExchange := Stub_InterlockedCompareExchange;
  {$ENDIF}
  {$IFDEF WINDOWS}
  GetTickCount64 := Stub_GetTickCount64;
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
  mach_timebase_info(GMachTimeBaseInfo);
    {$ENDIF}
  {$ENDIF}

{$IFNDEF DOTNET}
finalization
  FreeAndNil(GIdPorts);
{$ENDIF}

end.

