//types from SDL_stdinc.h
type

  TSDL_Bool = (SDL_FALSE = 0,SDL_TRUE = 1);

  DWord = LongWord;

  PUInt8Array = ^TUInt8Array;
  PUInt8 = ^UInt8;    
  PPUInt8 = ^PUInt8;
  UInt8 = Byte;
  {$EXTERNALSYM UInt8}
  TUInt8Array = array [0..MAXINT shr 1] of UInt8;

  PUInt16 = ^UInt16;
  UInt16 = Word;
  {$EXTERNALSYM UInt16}

  PSInt8 = ^SInt8;
  SInt8 = ShortInt;
  {$EXTERNALSYM SInt8}

  PSInt16 = ^SInt16;
  SInt16 = SmallInt;
  {$EXTERNALSYM SInt16}

  PUInt32 = ^UInt32;
  UInt32 = Cardinal;
  {$EXTERNALSYM UInt32}

  PSInt32 = ^SInt32;
  SInt32 = LongInt;
  {$EXTERNALSYM SInt32}

  PFloat = ^Float;
  PInt = ^LongInt;

  PShortInt = ^ShortInt;

  {$IFNDEF Has_Int64}
  PUInt64 = ^UInt64;
  UInt64 = record
    hi: UInt32;
    lo: UInt32;
  end;
  {$EXTERNALSYM UInt64}

  PInt64 = ^Int64;
  Int64 = record
    hi: UInt32;
    lo: UInt32;
  end;
  {$EXTERNALSYM Int64}

  PSInt64 = ^SInt64;
  SInt64 = Int64;
  {$EXTERNALSYM SInt64}
  {$ELSE}
  PSInt64 = ^SInt64;
  SInt64 = Int64;
  {$ENDIF}

  {$IFNDEF WIN64}
    size_t = UInt32;
  {$ELSE}
    size_t = UInt64;
  {$ENDIF}
  {$EXTERNALSYM SIZE_T}

  Float = Single;
  {$EXTERNALSYM Float}

{**
*  Free memory returned by functions like SDL_GetBasePath(), SDL_GetPrefPath(), etc.
*}
procedure SDL_free(mem: Pointer); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_free' {$ENDIF} {$ENDIF};
