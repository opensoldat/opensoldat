//from "sdl_rwops"

const
  {* RWops Types *}
  SDL_RWOPS_UNKNOWN	  = 0;	{* Unknown stream type *}
  SDL_RWOPS_WINFILE	  = 1;	{* Win32 file *}
  SDL_RWOPS_STDFILE	  = 2;	{* Stdio file *}
  SDL_RWOPS_JNIFILE	  = 3;	{* Android asset *}
  SDL_RWOPS_MEMORY    =	4;	{* Memory stream *}
  SDL_RWOPS_MEMORY_RO =	5;	{* Read-Only memory stream *}

type
  PSDL_RWops = ^TSDL_RWops;

  {**
   * This is the read/write operation structure -- very basic.
   *}

  {**
   *  Return the size of the file in this rwops, or -1 if unknown
   *}
  TSize = function(context: PSDL_RWops): SInt64; {$IFNDEF GPC} cdecl; {$ENDIF}

  {**
   *  Seek to offset relative to whence, one of stdio's whence values:
   *  RW_SEEK_SET, RW_SEEK_CUR, RW_SEEK_END
   *
   *  the final offset in the data stream, or -1 on error.
   *}
  TSeek = function(context: PSDL_RWops; offset: SInt64; whence: SInt32): SInt64; {$IFNDEF GPC} cdecl; {$ENDIF}
                   
  {**
   *  Read up to maxnum objects each of size size from the data
   *  stream to the area pointed at by ptr.
   *
   *  the number of objects read, or 0 at error or end of file.
   *}

   TRead = function(context: PSDL_RWops; ptr: Pointer; size: size_t; maxnum: size_t): size_t; {$IFNDEF GPC} cdecl; {$ENDIF}

  {**
   *  Write exactly num objects each of size size from the area
   *  pointed at by ptr to data stream.
   *  
   *  the number of objects written, or 0 at error or end of file.
   *}
	
   TWrite = function(context: PSDL_RWops; const ptr: Pointer; size: size_t; num: size_t): size_t; {$IFNDEF GPC} cdecl; {$ENDIF}
	
  {**
   *  Close and free an allocated SDL_RWops structure.
   *  
   *  0 if successful or -1 on write error when flushing data.
   *}

  TClose =  function(context: PSDL_RWops): SInt32; {$IFNDEF GPC} cdecl; {$ENDIF}

  TStdio = record
    autoclose: TSDL_Bool;
	fp: file;
  end;
  
  TMem = record
    base: PUInt8;
	here: PUInt8;
	stop: PUInt8;
  end;
  
  TUnknown = record
    data1: Pointer;
  end;

  TAndroidIO = record
    fileNameRef: Pointer;
    inputStreamRef: Pointer;
    readableByteChannelRef: Pointer;
    readMethod: Pointer;
    assetFileDescriptorRef: Pointer;
    position: LongInt;
    size: LongInt;
    offset: LongInt;
    fd: SInt32;
  end;

  TWindowsIOBuffer = record
    data: Pointer;
	size: size_t;
	left: size_t;
  end;

  TWindowsIO = record
    append: TSDL_Bool;
    h: Pointer;
    buffer: TWindowsIOBuffer;
  end;

  TSDL_RWops = packed record
    size: TSize;
    seek: TSeek;
    read: TRead;
    write: TWrite;
    close: TClose;

    _type: UInt32;

	case Integer of
	  0: (stdio: TStdio);
	  1: (mem: TMem);
	  2: (unknown: TUnknown);
	  {$IFDEF ANDROID}
	  3: (androidio: TAndroidIO);
	  {$ENDIF}
	  {$IFDEF WINDOWS}
	  3: (windowsio: TWindowsIO);
	  {$ENDIF}
  end;

  {**
   *  RWFrom functions
   *
   *  Functions to create SDL_RWops structures from various data streams.
   *}

function SDL_RWFromFile(const _file: PAnsiChar; const mode: PAnsiChar): PSDL_RWops; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RWFromFile' {$ENDIF} {$ENDIF};

  {function SDL_RWFromFP(fp: file; autoclose: TSDL_Bool): PSDL_RWops; cdecl; external SDL_LibName;} //don't know if this works

function SDL_RWFromFP(fp: Pointer; autoclose: TSDL_Bool): PSDL_RWops; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RWFromFP' {$ENDIF} {$ENDIF};

function SDL_RWFromMem(mem: Pointer; size: SInt32): PSDL_RWops; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RWFromMem' {$ENDIF} {$ENDIF};
function SDL_RWFromConstMem(const mem: Pointer; size: SInt32): PSDL_RWops; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RWFromConstMem' {$ENDIF} {$ENDIF};

{*RWFrom functions*}


function SDL_AllocRW: PSDL_RWops; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_AllocRW' {$ENDIF} {$ENDIF};
procedure SDL_FreeRW(area: PSDL_RWops); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_FreeRW' {$ENDIF} {$ENDIF};

const
  RW_SEEK_SET = 0;       {**< Seek from the beginning of data *}
  RW_SEEK_CUR = 1;       {**< Seek relative to current read point *}
  RW_SEEK_END = 2;       {**< Seek relative to the end of data *}

  {**
   *  Read/write macros
   *
   *  Macros to easily read and write from an SDL_RWops structure.
   *}

  function SDL_RWsize(ctx: PSDL_RWops): SInt64; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
  function SDL_RWseek(ctx: PSDL_RWops; offset: SInt64; whence: SInt32): SInt64; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
  function SDL_RWtell(ctx: PSDL_RWops): SInt64; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
  function SDL_RWread(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
  function SDL_RWwrite(ctx: PSDL_RWops; ptr: Pointer; size: size_t; n: size_t): size_t; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
  function SDL_RWclose(ctx: PSDL_RWops): SInt32; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
  { Read/write macros }


  {**
   *  Read endian functions
   *
   *  Read an item of the specified endianness and return in native format.
   *}

function SDL_ReadU8(src: PSDL_RWops): UInt8 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadU8' {$ENDIF} {$ENDIF};
function SDL_ReadLE16(src: PSDL_RWops): UInt16 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadLE16' {$ENDIF} {$ENDIF};
function SDL_ReadBE16(src: PSDL_RWops): UInt16 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadBE16' {$ENDIF} {$ENDIF};
function SDL_ReadLE32(src: PSDL_RWops): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadLE32' {$ENDIF} {$ENDIF};
function SDL_ReadBE32(src: PSDL_RWops): UInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadBE32' {$ENDIF} {$ENDIF};
function SDL_ReadLE64(src: PSDL_RWops): UInt64 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadLE64' {$ENDIF} {$ENDIF};
function SDL_ReadBE64(src: PSDL_RWops): UInt64 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ReadBE64' {$ENDIF} {$ENDIF};

  {*Read endian functions*}

  {**
   *  Write endian functions
   *
   *  Write an item of native format to the specified endianness.
   *}

function SDL_WriteU8(dst: PSDL_RWops; value: UInt8): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteU8' {$ENDIF} {$ENDIF};
function SDL_WriteLE16(dst: PSDL_RWops; value: UInt16): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteLE16' {$ENDIF} {$ENDIF};
function SDL_WriteBE16(dst: PSDL_RWops; value: UInt16): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteBE16' {$ENDIF} {$ENDIF};
function SDL_WriteLE32(dst: PSDL_RWops; value: UInt32): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteLE32' {$ENDIF} {$ENDIF};
function SDL_WriteBE32(dst: PSDL_RWops; value: UInt32): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteBE32' {$ENDIF} {$ENDIF};
function SDL_WriteLE64(dst: PSDL_RWops; value: UInt64): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteLE64' {$ENDIF} {$ENDIF};
function SDL_WriteBE64(dst: PSDL_RWops; value: UInt64): size_t cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_WriteBE64' {$ENDIF} {$ENDIF};
  { Write endian functions }
