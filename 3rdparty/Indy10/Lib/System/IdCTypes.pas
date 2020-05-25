unit IdCTypes;

// TODO: deprecate this unit and move the declarations to the IdGlobal unit.

interface

{$I IdCompilerDefines.inc}

{This unit should not contain ANY program code.  It is meant to be extremely 
thin.  The idea is that the unit will contain type mappings that used for headers
and API calls using the headers.  The unit is here because in cross-platform
headers, the types may not always be the same as they would for Win32 on x86
Intel architecture.  We also want to be completely compatiable with Borland
Delphi for Win32.}

{$IFDEF HAS_UNIT_ctypes}
uses
  ctypes;
{$ENDIF}

{
IMPORTANT!!!

The types below are defined to hide architecture differences for various C++
types while also making this header compile with Borland Delphi.

}
type 
  {$IFDEF FPC}

  TIdC_LONG  = cLong;
  PIdC_LONG  = pcLong;
  TIdC_ULONG = cuLong;
  PIdC_ULONG = pculong;

  TIdC_LONGLONG = clonglong;
  PIdC_LONGLONG = pclonglong;
  TIdC_ULONGLONG = culonglong;
  PIdC_ULONGLONG = pculonglong;

  TIdC_SHORT = cshort;
  PIdC_SHORT = pcshort;
  TIdC_USHORT = cuShort;
  PIdC_USHORT = pcuShort;
  
  TIdC_INT   = cInt;
  PIdC_INT   = pcInt;
  TIdC_UINT  = cUInt;
  PIdC_UINT  = pcUInt;

  TIdC_SIGNED = csigned;
  PIdC_SIGNED = pcsigned;  
  TIdC_UNSIGNED = cunsigned;
  PIdC_UNSIGNED = pcunsigned;

  TIdC_INT8 = cint8;
  PIdC_INT8  = pcint8;
  TIdC_UINT8 = cuint8;
  PIdC_UINT8 = pcuint8;

  TIdC_INT16 = cint16;
  PIdC_INT16 = pcint16;
  TIdC_UINT16 = cuint16;
  PIdC_UINT16 = pcuint16;

  TIdC_INT32 = cint32;
  PIdC_INT32 = pcint32;
  TIdC_UINT32 = cint32;
  PIdC_UINT32 = pcuint32;

  TIdC_INT64 = cint64;
  PIdC_INT64 = pcint64;
  TIdC_UINT64 = cuint64;
  PIdC_UINT64 = pcuint64;

  TIdC_FLOAT = cfloat;
  PIdC_FLOAT = pcfloat;
  TIdC_DOUBLE = cdouble;
  PIdC_DOUBLE = pcdouble;
  TIdC_LONGDOUBLE = clongdouble;
  PIdC_LONGDOUBLE =  pclongdouble;

  {$ELSE}

  //this is necessary because Borland still doesn't support QWord
  // (unsigned 64bit type).
  {$IFNDEF HAS_QWord}
  qword = {$IFDEF HAS_UInt64}UInt64{$ELSE}Int64{$ENDIF};
  {$ENDIF}

  TIdC_LONG  = LongInt;
  PIdC_LONG  = ^TIdC_LONG;
  TIdC_ULONG = LongWord;
  PIdC_ULONG = ^TIdC_ULONG;

  TIdC_LONGLONG = Int64;
  PIdC_LONGLONG = ^TIdC_LONGLONG;
  TIdC_ULONGLONG = QWord;
  PIdC_ULONGLONG = ^TIdC_ULONGLONG;

  TIdC_SHORT = Smallint;
  PIdC_SHORT = ^TIdC_SHORT;
  TIdC_USHORT = Word;
  PIdC_USHORT = ^TIdC_USHORT;

  TIdC_INT   = Integer;
  PIdC_INT   = ^TIdC_INT;
  TIdC_UINT  = Cardinal;
  PIdC_UINT  = ^TIdC_UINT;

  TIdC_SIGNED = Integer;
  PIdC_SIGNED = ^TIdC_SIGNED;
  TIdC_UNSIGNED = Cardinal;
  PIdC_UNSIGNED = ^TIdC_UNSIGNED;

  TIdC_INT8 = Shortint;
  PIdC_INT8  = ^TIdC_INT8;
  TIdC_UINT8 = Byte;
  PIdC_UINT8 = ^TIdC_UINT8;

  TIdC_INT16 = Smallint;
  PIdC_INT16 = ^TIdC_INT16;
  TIdC_UINT16 = Word;
  PIdC_UINT16 = ^TIdC_UINT16;

  TIdC_INT32 = Integer;
  PIdC_INT32 = ^TIdC_INT32;
  TIdC_UINT32 = Cardinal;
  PIdC_UINT32 = ^TIdC_UINT32;

  TIdC_INT64 = Int64;
  PIdC_INT64 = ^TIdC_INT64;
  TIdC_UINT64 = QWord;
  PIdC_UINT64 = ^TIdC_UINT64;

  TIdC_FLOAT = Single;
  PIdC_FLOAT = ^TIdC_FLOAT;
  TIdC_DOUBLE = Double;
  PIdC_DOUBLE = ^TIdC_DOUBLE;
  TIdC_LONGDOUBLE = Extended;
  PIdC_LONGDOUBLE = ^TIdC_LONGDOUBLE;

  //Some headers require this in D5 or earlier.
  //FreePascal already has this in its system unit.
  {$IFNDEF HAS_PByte}PByte = ^Byte;{$ENDIF}
  {$IFNDEF HAS_PWord}PWord = ^Word;{$ENDIF}

  {$ENDIF}

implementation

end.
