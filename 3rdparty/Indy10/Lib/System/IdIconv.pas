unit IdIconv;

interface

{$I IdCompilerDefines.inc}

uses
  {$IFDEF FPC}
  DynLibs,  
  {$ENDIF}
  IdCTypes,
  IdException
  {$IFDEF USE_BASEUNIX}
  ,UnixType
  {$ENDIF}
  {$IFDEF WINDOWS}
  ,Windows
  {$ENDIF}
  ;

{.$DEFINE STATICLOAD_ICONV}
//These should be defined in libc.pas.
type
  {$IFDEF WINDOWS}
  {$EXTERNALSYM SIZE_T}
    {$IFDEF CPU64}
  size_t = QWord;
    {$ELSE}
  size_t = DWord;
    {$ENDIF}
  Psize_t = ^size_t;
  {$ENDIF}

  Piconv_t = ^iconv_t;
  iconv_t = Pointer;

{$IFNDEF STATICLOAD_ICONV}
//   This function is a possible cancellation points and therefore not
//   marked with __THROW.  */
//extern iconv_t iconv_open (__const char *__tocode, __const char *__fromcode);
  TIdiconv_open = function (__tocode : PAnsiChar; __fromcode : PAnsiChar) : iconv_t;  cdecl;
///* Convert at most *INBYTESLEFT bytes from *INBUF according to the
//   code conversion algorithm specified by CD and place up to
//   *OUTBYTESLEFT bytes in buffer at *OUTBUF.  */
//extern size_t iconv (iconv_t __cd, char **__restrict __inbuf,
//		     size_t *__restrict __inbytesleft,
//		     char **__restrict __outbuf,
//		     size_t *__restrict __outbytesleft);
  TIdiconv = function (__cd : iconv_t; __inbuf : PPAnsiChar;
                    __inbytesleft : Psize_t;
		    __outbuf : PPAnsiChar;
		    __outbytesleft : Psize_t ) : size_t; cdecl;
//   This function is a possible cancellation points and therefore not
//   marked with __THROW.  */
//extern int iconv_close (iconv_t __cd);
  TIdiconv_close = function (__cd : iconv_t) : TIdC_INT; cdecl;

type
  EIdIconvStubError = class(EIdException)
  protected
    FError : LongWord;
    FErrorMessage : String;
    FTitle : String;
  public
    constructor Build(const ATitle : String; AError : LongWord);
    property Error : LongWord read FError;
    property ErrorMessage : String read FErrorMessage;
    property Title : String read FTitle;
  end;
  
var
  iconv_open  : TIdiconv_open = nil;
  iconv       : TIdiconv = nil;
  iconv_close : TIdiconv_close = nil;
{$ENDIF}

{$IFDEF WIN32_OR_WIN64}
//errno.h constants that are needed for this and possibly other API's.
//It's here only because it seems to be the most sensible place to put it.
//These are defined in other operating systems.

const
  {$EXTERNALSYM EPERM}
  EPERM          =  1;
  {$EXTERNALSYM ENOENT}
  ENOENT         =  2;
  {$EXTERNALSYM ESRCH}
  ESRCH          =  3;
  {$EXTERNALSYM EINTR}
  EINTR          =  4;
  {$EXTERNALSYM EIO}
  EIO            =  5;
  {$EXTERNALSYM ENXIO}
  ENXIO          =  6;
  {$EXTERNALSYM E2BIG}
  E2BIG          =  7;
  {$EXTERNALSYM ENOEXEC}
  ENOEXEC        =  8;
  {$EXTERNALSYM EBADF}
  EBADF          =  9;
  {$EXTERNALSYM ECHILD}
  ECHILD         = 10;
  {$EXTERNALSYM EAGAIN}
  EAGAIN         = 11;
  {$EXTERNALSYM ENOMEM}
  ENOMEM         = 12;
  {$EXTERNALSYM EACCES}
  EACCES         = 13;
  {$EXTERNALSYM EFAULT}
  EFAULT         = 14;
  {$EXTERNALSYM EBUSY}
  EBUSY          = 16;
  {$EXTERNALSYM EEXIST}
  EEXIST         = 17;
  {$EXTERNALSYM EXDEV}
  EXDEV          = 18;
  {$EXTERNALSYM ENODEV}
  ENODEV         = 19;
  {$EXTERNALSYM ENOTDIR}
  ENOTDIR        = 20;
  {$EXTERNALSYM EISDIR}
  EISDIR         = 21;
  {$EXTERNALSYM EINVAL}
  EINVAL         = 22;
  {$EXTERNALSYM ENFILE}
  ENFILE         = 23;
  {$EXTERNALSYM EMFILE}
  EMFILE         = 24;
  {$EXTERNALSYM ENOTTY}
  ENOTTY         = 25;
  {$EXTERNALSYM EFBIG}
  EFBIG          = 27;
  {$EXTERNALSYM ENOSPC}
  ENOSPC         = 28;
  {$EXTERNALSYM ESPIPE}
  ESPIPE         = 29;
  {$EXTERNALSYM EROFS}
  EROFS          = 30;
  {$EXTERNALSYM EMLINK}
  EMLINK         = 31;
  {$EXTERNALSYM EPIPE}
  EPIPE          = 32;
  {$EXTERNALSYM EDOM}
  EDOM           = 33;
  {$EXTERNALSYM ERANGE}
  ERANGE         = 34;
  {$EXTERNALSYM EDEADLK}
  EDEADLK        = 36;
  {$EXTERNALSYM ENAMETOOLONG}
  ENAMETOOLONG   = 38;
  {$EXTERNALSYM ENOLCK}
  ENOLCK         = 39;
  {$EXTERNALSYM ENOSYS}
  ENOSYS         = 40;
  {$EXTERNALSYM ENOTEMPTY}
  ENOTEMPTY      = 41;
  {$EXTERNALSYM EILSEQ}
  EILSEQ         = 42;

type
  EIdMSVCRTStubError = class(EIdException)
  protected
    FError : TIdC_INT;
    FErrorMessage : String;
    FTitle : String;
  public
    constructor Build(const ATitle : String; AError : TIdC_INT);
    property Error : TIdC_INT read FError;
    property ErrorMessage : String read FErrorMessage;
    property Title : String read FTitle;
  end;
{$ENDIF}

const
  FN_ICONV_OPEN = 'iconv_open';  {Do not localize}
  FN_ICONV = 'iconv';   {Do not localize}
  FN_ICONV_CLOSE = 'iconv_close';  {Do not localize}
  {$IFDEF UNIX}
  LIBC = 'libc.so.6';  {Do not localize}
  LICONV = 'libiconv.so';  {Do not localize}
  {$ELSE}
  // TODO: support static linking, such as via the "win_iconv" library
    {$IFDEF WINDOWS}
  //http://yukihiro.nakadaira.googlepages.com/ seems to use the iconv.dll name.
  LICONV = 'iconv.dll';   {Do not localize}
  LICONV_ALT = 'libiconv.dll';   {Do not localize}
  LIBMSVCRTL = 'msvcrt.dll';  {Do not localize}
    {$ENDIF}
  {$ENDIF}

function Load : Boolean;
procedure Unload;
function Loaded : Boolean;

{$IFDEF STATICLOAD_ICONV}
function iconv_open(__tocode : PAnsiChar; __fromcode : PAnsiChar) : iconv_t; cdecl;
  external LICONV name FN_ICONV_OPEN;

function iconv(__cd : iconv_t; __inbuf : PPAnsiChar;
                    __inbytesleft : Psize_t;
		    __outbuf : PPAnsiChar;
		    __outbytesleft : Psize_t ) : size_t; cdecl;
  external LICONV name FN_ICONV;

function iconv_close(__cd : iconv_t) : TIdC_INT; cdecl;
  external LICONV name FN_ICONV_CLOSE;
{$ENDIF}

{
From http://gettext.sourceforge.net/

Dynamic linking to iconv
Note that the iconv function call in libiconv stores its error, if it fails,
in the stdc library's errno. iconv.dll links to msvcrt.dll, and stores the error
in its exported errno symbol (this is actually a memory location with an
exported accessor function). If your code does not link against msvcrt.dll, you
may wish to import this accessor function specifically to get iconv failure
codes. This is particularly important for Visual C++ projects, which are likely
to link to msvcrtd.dll in Debug configuration, rather than msvcrt.dll. I have
written a C++ wrapper for iconv use, which does this, and I will be adding it to
WinMerge (on sourceforge) shortly.
}
{$IFDEF WIN32_OR_WIN64}
var
  errno : function : PIdC_INT; cdecl;

function errnoStr(const AErrNo : TIdC_INT) : String;
{$ENDIF}

implementation

{$IFNDEF STATICLOAD_ICONV}
uses
  IdResourceStrings, SysUtils;

var
  {$IFDEF UNIX}
  hIconv: HModule = nilhandle;
  {$ELSE}
  hIconv: THandle = 0;
  {$ENDIF}
{$ENDIF}

{$IFDEF WIN32_OR_WIN64}
var
  hmsvcrt : THandle = 0;

function Stub_errno : PIdC_INT; cdecl; forward;
{$ENDIF}
  
{$IFNDEF STATICLOAD_ICONV}
constructor EIdIconvStubError.Build(const ATitle : String; AError : LongWord);
begin
  FTitle := ATitle;
  FError := AError;
  if AError = 0 then begin
    inherited Create(ATitle);
  end else
  begin
    FErrorMessage := SysUtils.SysErrorMessage(AError);
    inherited Create(ATitle + ': ' + FErrorMessage);    {Do not Localize}
  end;

end;

function Fixup(const AName: string): Pointer;
begin
  if hIconv = 0 then begin
    if not Load then begin
      raise EIdIconvStubError.Build(Format(RSIconvCallError, [AName]), 0);
    end;
  end;
  Result := GetProcAddress(hIconv, PChar(AName));
  {
  IMPORTANT!!!

  GNU libiconv for Win32 might be compiled with the LIBICONV_PLUG define.
  If that's the case, we will have to load the functions with a "lib" prefix.

  IOW, CYA!!!
  }
  if Result = nil then begin
    Result := GetProcAddress(hIconv, PChar('lib'+AName));
    if Result = nil then begin
      raise EIdIconvStubError.Build(Format(RSIconvCallError, [AName]), 10022);
    end;
  end;
end;

{stubs that automatically load the iconv library and then fixup the functions.}

function Stub_iconv_open(__tocode : PAnsiChar; __fromcode : PAnsiChar) : iconv_t;  cdecl;
begin
  iconv_open := Fixup(FN_ICONV_OPEN);
  Result := iconv_open(__tocode, __fromcode);
end;

function stub_iconv(__cd : iconv_t; __inbuf : PPAnsiChar; 
                    __inbytesleft : Psize_t; 
		    __outbuf : PPAnsiChar;
		    __outbytesleft : Psize_t ) : size_t; cdecl;
begin
  iconv := Fixup(FN_ICONV);
  Result := iconv(__cd,__inbuf,__inbytesleft,__outbuf,__outbytesleft);
end;

function stub_iconv_close(__cd : iconv_t) : TIdC_INT; cdecl;
begin
  iconv_close := Fixup(FN_ICONV_CLOSE);
  Result := iconv_close(__cd);
end;

{end stub sections}

{$ENDIF}

procedure InitializeStubs;
begin
{$IFNDEF STATICLOAD_ICONV}
  iconv_open  := Stub_iconv_open;
  iconv       := Stub_iconv;
  iconv_close := Stub_iconv_close;
{$ENDIF}
{$IFDEF WIN32_OR_WIN64}
  errno       := Stub_errno;
{$ENDIF}
end;

function Load : Boolean;
{$IFDEF STATICLOAD_ICONV}
  {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}
begin
{$IFDEF STATICLOAD_ICONV}
  Result := True;
{$ELSE}
  if not Loaded then begin
    //In Windows, you should use SafeLoadLibrary instead of the LoadLibrary API
    //call because LoadLibrary messes with the FPU control word.
    {$IFDEF WINDOWS}
    hIconv := SafeLoadLibrary(LICONV);
    if hIconv = 0 then begin
      hIconv := SafeLoadLibrary(LICONV_ALT);
    end;
    {$ELSE}
      {$IFDEF UNIX}
    hIconv := LoadLibrary(LICONV);
    if hIconv = NilHandle then  begin
      hIconv := LoadLibrary(LIBC);
    end;
      {$ELSE}
    hIconv := LoadLibrary(LICONV);
      {$ENDIF}
    {$ENDIF}
    Result := Loaded;
  end;
{$ENDIF}
end;

procedure Unload;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
{$IFNDEF STATICLOAD_ICONV}
  if Loaded then begin
    FreeLibrary(hIconv);
    hIconv := 0;
  end;
{$ENDIF}
{$IFDEF WIN32_OR_WIN64}
  if hmsvcrt <> 0 then begin
    FreeLibrary(hmsvcrt);
    hmsvcrt := 0;
  end;
{$ENDIF}
  InitializeStubs;
end;

function Loaded : Boolean;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
{$IFDEF STATICLOAD_ICONV}
  Result := True;
{$ELSE}
  Result := (hIconv <> 0);
{$ENDIF}
end;

{$IFDEF WIN32_OR_WIN64}
const
  FN_errno = '_errno';

constructor EIdMSVCRTStubError.Build(const ATitle : String; AError : TIdC_INT);
begin
  FTitle := ATitle;
  FError := AError;
  if AError = 0 then begin
    inherited Create(ATitle);
  end else
  begin
    FErrorMessage := errnoStr(AError);
    inherited Create(ATitle + ': ' + FErrorMessage);    {Do not Localize}
  end;
end;

function errnoStr(const AErrNo : TIdC_INT) : String;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
 case AErrNo of
  EPERM        : Result := 'EPERM';
  ENOENT       : Result := 'ENOENT';
  ESRCH        : Result := 'ESRCH';
  EINTR        : Result := 'EINTR';
  EIO          : Result := 'EIO';
  ENXIO        : Result := 'ENXIO';
  E2BIG        : Result := 'E2BIG';
  ENOEXEC      : Result := 'ENOEXEC';
  EBADF        : Result := 'EBADF';
  ECHILD       : Result := 'ECHILD';
  EAGAIN       : Result := 'EAGAIN';
  ENOMEM       : Result := 'ENOMEM';
  EACCES       : Result := 'EACCES';
  EFAULT       : Result := 'EFAULT';
  EBUSY        : Result := 'EBUSY';
  EEXIST       : Result := 'EEXIST';
  EXDEV        : Result := 'EXDEV';
  ENODEV       : Result := 'ENODEV';
  ENOTDIR      : Result := 'ENOTDIR';
  EISDIR       : Result := 'EISDIR';
  EINVAL       : Result := 'EINVAL';
  ENFILE       : Result := 'ENFILE';
  EMFILE       : Result := 'EMFILE';
  ENOTTY       : Result := 'ENOTTY';
  EFBIG        : Result := 'EFBIG';
  ENOSPC       : Result := 'ENOSPC';
  ESPIPE       : Result := 'ESPIPE';
  EROFS        : Result := 'EROFS';
  EMLINK       : Result := 'EMLINK';
  EPIPE        : Result := 'EPIPE';
  EDOM         : Result := 'EDOM';
  ERANGE       : Result := 'ERANGE';
  EDEADLK      : Result := 'EDEADLK';
  ENAMETOOLONG : Result := 'ENAMETOOLONG';
  ENOLCK       : Result := 'ENOLCK';
  ENOSYS       : Result := 'ENOSYS';
  ENOTEMPTY    : Result := 'ENOTEMPTY';
  EILSEQ       : Result := 'EILSEQ';
  else
    Result := '';
  end;
end;

function Stub_errno : PIdC_INT; cdecl;
begin
  if hmsvcrt = 0 then begin
    hmsvcrt := SafeLoadLibrary(LIBMSVCRTL);
    if hmsvcrt = 0 then begin
      raise EIdMSVCRTStubError.Build('Failed to load ' + LIBMSVCRTL, 0);
    end;
    errno := GetProcAddress(hmsvcrt, PChar(FN_errno));
    if not Assigned(errno) then begin
      errno := Stub_errno;
      raise EIdMSVCRTStubError.Build('Failed to load ' + FN_errno + ' in ' + LIBMSVCRTL, 0);
    end;
  end;
  Result := errno();
end;
{$ENDIF}

initialization
  InitializeStubs;

finalization
  Unload;

end.
