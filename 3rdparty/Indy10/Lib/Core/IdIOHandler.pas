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
  Rev 1.123    2/8/05 5:27:06 PM  RLebeau
  Bug fix for ReadLn().

  Added try..finally block to ReadLnSplit().

  Rev 1.122    1/27/05 3:09:30 PM  RLebeau
  Updated AllData() to call ReadFromSource() directly instead of using
  CheckForDataOnSource(), since ReadFromSource() can return a disconnect
  conditon.  When data is in the InputBuffer, Connected() always return True
  even if the socket is actually disconnected.

  Rev 1.121    12/21/04 3:21:40 AM  RLebeau
  Removed compiler warning

  Rev 1.120    17/12/2004 17:11:28  ANeillans
  Compiler fix

  Rev 1.119    12/12/04 2:23:52 PM  RLebeau
  Added WriteRFCStrings() method

  Rev 1.118    12/11/2004 9:04:50 PM  DSiders
  Fixed comparison error in WaitFor.

  Rev 1.117    12/10/04 2:00:24 PM  RLebeau
  Updated WaitFor() to not return more data than actually needed.

  Updated AllData() to not concatenate the Result on every iteration of the
  loop.

  Rev 1.116    11/29/04 10:37:18 AM  RLebeau
  Updated write buffering methods to prevent Access Violations when used
  incorrectly.

  Rev 1.115    11/4/04 12:41:08 PM  RLebeau
  Bug fix for ReadLn()

  Rev 1.114    10/26/2004 8:43:00 PM  JPMugaas
  Should be more portable with new references to TIdStrings and TIdStringList.

  Rev 1.113    27.08.2004 21:58:18  Andreas Hausladen
  Speed optimization ("const" for string parameters)

  Rev 1.112    8/2/04 5:49:20 PM  RLebeau
  Moved ConnectTimeout over from TIdIOHandlerSocket

  Rev 1.111    2004.08.01 19:36:14  czhower
  Code optimization to WriteFile

  Rev 1.110    7/24/04 12:53:54 PM  RLebeau
  Compiler fix for WriteFile()

  Rev 1.109    7/23/04 6:39:14 PM  RLebeau
  Added extra exception handling to WriteFile()

  Rev 1.108    7/21/2004 5:45:10 PM  JPMugaas
  Updated with Remy's change.  This should work better and fix a problem with
  looping with ReadStream and ReadUntilDisconnect.

  Rev 1.107    7/21/2004 12:22:18 PM  BGooijen
  Reverted back 2 versions

  Rev 1.104    6/29/04 12:16:16 PM  RLebeau
  Updated ReadChar() to call ReadBytes() directly instead of ReadString()

  Rev 1.103    6/17/04 3:01:56 PM  RLebeau
  Changed ReadStream() to not extract too many bytes from the InputBuffer when
  an error occurs

  Rev 1.102    6/12/04 11:36:44 AM  RLebeau
  Changed ReadString() to pass the ABytes parameter to ReadBytes() instead of
  the LBuf length

  Rev 1.100    6/10/2004 6:52:12 PM  JPMugaas
  Regeneration to fix a bug in the package generator that I created.  OOPS!!!

  Rev 1.99    6/9/04 7:36:26 PM  RLebeau
  ReadString() bug fix

  Rev 1.98    07/06/2004 20:55:36  CCostelloe
  Fix for possible memory leak.

  Rev 1.97    5/29/04 10:46:24 PM  RLebeau
  Updated AllData() to only append values to the result when there is actual
  data in the buffer.

  Rev 1.96    29/05/2004 21:07:40  CCostelloe
  Bug fix (may need more investigation)

  Rev 1.95    2004.05.20 1:39:54 PM  czhower
  Last of the IdStream updates

  Rev 1.94    2004.05.20 12:34:22 PM  czhower
  Removed more non .NET compatible stream read and writes

  Rev 1.93    2004.05.20 11:39:02 AM  czhower
  IdStreamVCL

  Rev 1.92    5/3/2004 12:57:00 PM  BGooijen
  Fixes for 0-based

  Rev 1.91    2004.05.03 11:15:44 AM  czhower
  Changed Find to IndexOf and made 0 based to be consistent.

  Rev 1.90    4/24/04 12:40:04 PM  RLebeau
  Added Write() overload for Char type.

  Rev 1.89    4/18/2004 11:58:00 PM  BGooijen
  ReadBytes with count=-1 reads everything available, ( and waits ReadTimeOut
  time for data)

  Rev 1.88    4/18/04 2:44:24 PM  RLebeau
  Read/write support for Int64 values

  Rev 1.87    2004.04.18 12:51:58 AM  czhower
  Big bug fix with server disconnect and several other bug fixed that I found
  along the way.

  Rev 1.86    2004.04.16 11:30:28 PM  czhower
  Size fix to IdBuffer, optimizations, and memory leaks

  Rev 1.85    2004.04.08 7:06:46 PM  czhower
  Peek support.

  Rev 1.84    2004.04.08 3:56:28 PM  czhower
  Fixed bug with Intercept byte count. Also removed Bytes from Buffer.

  Rev 1.83    2004.04.08 2:08:00 AM  czhower
  Saved before checkin this time...

  Rev 1.82    7/4/2004 4:08:46 PM  SGrobety
  Re-introduce the IOHandler.MaxCapturedLines property

  Rev 1.81    2004.04.07 3:59:46 PM  czhower
  Bug fix for WriteDirect.

  Rev 1.79    2004.03.07 11:48:38 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.78    2004.03.03 11:54:58 AM  czhower
  IdStream change

  Rev 1.77    2004.03.02 2:47:08 PM  czhower
  .Net overloads

  Rev 1.76    2004.03.01 5:12:28 PM  czhower
  -Bug fix for shutdown of servers when connections still existed (AV)
  -Implicit HELP support in CMDserver
  -Several command handler bugs
  -Additional command handler functionality.

  Rev 1.75    2004.02.03 4:16:44 PM  czhower
  For unit name changes.

  Rev 1.74    2004.01.21 9:36:00 PM  czhower
  .Net overload

  Rev 1.73    2004.01.21 12:19:58 AM  czhower
  .Readln overload for .net

  Rev 1.72    2004.01.20 10:03:26 PM  czhower
  InitComponent

  Rev 1.71    1/11/2004 5:51:04 PM  BGooijen
  Added AApend parameter to ReadBytes

  Rev 1.70    12/30/2003 7:17:56 PM  BGooijen
  .net

  Rev 1.69    2003.12.28 1:05:54 PM  czhower
  .Net changes.

  Rev 1.68    2003.12.28 11:53:28 AM  czhower
  Removed warning in .net.

  Rev 1.67    2003.11.29 10:15:30 AM  czhower
  InternalBuffer --> InputBuffer for consistency.

  Rev 1.66    11/23/03 1:46:28 PM  RLebeau
  Removed "var" specifier from TStrings parameter of ReadStrings().

    Rev 1.65    11/4/2003 10:27:56 PM  DSiders
  Removed exceptions moved to IdException.pas.

  Rev 1.64    2003.10.24 10:44:52 AM  czhower
  IdStream implementation, bug fixes.

  Rev 1.63    10/22/03 2:05:40 PM  RLebeau
  Fix for TIdIOHandler::Write(TStream) where it was not reading the stream into
  the TIdBytes correctly.

  Rev 1.62    10/19/2003 5:55:44 PM  BGooijen
  Fixed todo in PerformCapture

  Rev 1.61    2003.10.18 12:58:50 PM  czhower
  Added comment

  Rev 1.60    2003.10.18 12:42:04 PM  czhower
  Intercept.Disconnect is now called

    Rev 1.59    10/15/2003 7:39:28 PM  DSiders
  Added a formatted resource string for the exception raised in
  TIdIOHandler.MakeIOHandler.

  Rev 1.58    2003.10.14 1:26:50 PM  czhower
  Uupdates + Intercept support

  Rev 1.57    2003.10.11 5:48:22 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.56    9/10/2003 1:50:38 PM  SGrobety
  Removed all "const" keywords from boolean parameter interfaces. Might trigger
  changes in other units.

  Rev 1.55    10/5/2003 10:39:56 PM  BGooijen
  Write buffering

  Rev 1.54    10/4/2003 11:03:12 PM  BGooijen
  ReadStream, and functions with network ordering

  Rev 1.53    10/4/2003 7:10:46 PM  BGooijen
  ReadXXXXX

  Rev 1.52    10/4/2003 3:55:02 PM  BGooijen
  ReadString, and some Write functions

  Rev 1.51    04/10/2003 13:38:32  HHariri
  Write(Integer) support

  Rev 1.50    10/3/2003 12:09:30 AM  BGooijen
  DotNet

  Rev 1.49    2003.10.02 8:29:14 PM  czhower
  Changed names of byte conversion routines to be more readily understood and
  not to conflict with already in use ones.

  Rev 1.48    2003.10.02 1:18:50 PM  czhower
  Changed read methods to be overloaded and more consistent. Will break some
  code, but nearly all code that uses them is Input.

  Rev 1.47    2003.10.02 10:16:26 AM  czhower
  .Net

  Rev 1.46    2003.10.01 9:11:16 PM  czhower
  .Net

  Rev 1.45    2003.10.01 2:46:36 PM  czhower
  .Net

  Rev 1.42    2003.10.01 11:16:32 AM  czhower
  .Net

  Rev 1.41    2003.10.01 1:37:34 AM  czhower
  .Net

  Rev 1.40    2003.10.01 1:12:34 AM  czhower
  .Net

  Rev 1.39    2003.09.30 1:22:56 PM  czhower
  Stack split for DotNet

  Rev 1.38    2003.09.18 5:17:58 PM  czhower
  Implemented OnWork

  Rev 1.37    2003.08.21 10:43:42 PM  czhower
  Fix to ReadStream from Doychin

  Rev 1.36    08/08/2003 17:32:26  CCostelloe
  Removed "virtual" from function ReadLnSplit

  Rev 1.35    07/08/2003 00:25:08  CCostelloe
  Function ReadLnSplit added

  Rev 1.34    2003.07.17 1:05:12 PM  czhower
  More IOCP improvements.

  Rev 1.33    2003.07.14 11:00:50 PM  czhower
  More IOCP fixes.

  Rev 1.32    2003.07.14 12:54:30 AM  czhower
  Fixed graceful close detection if it occurs after connect.

  Rev 1.31    2003.07.10 7:40:24 PM  czhower
  Comments

  Rev 1.30    2003.07.10 4:34:56 PM  czhower
  Fixed AV, added some new comments

    Rev 1.29    7/1/2003 5:50:44 PM  BGooijen
  Fixed ReadStream

    Rev 1.28    6/30/2003 10:26:08 AM  BGooijen
  forgot to remove some code regarding to TIdBuffer.Find

    Rev 1.27    6/29/2003 10:56:26 PM  BGooijen
  Removed .Memory from the buffer, and added some extra methods

  Rev 1.26    2003.06.25 4:30:00 PM  czhower
  Temp hack fix for AV problem. Working on real solution now.

  Rev 1.25    23/6/2003 22:33:14  GGrieve
  fix CheckForDataOnSource - specify timeout

  Rev 1.24    23/6/2003 06:46:52  GGrieve
  allow block on checkForData

    Rev 1.23    6/4/2003 1:07:08 AM  BGooijen
  changed comment

    Rev 1.22    6/3/2003 10:40:34 PM  BGooijen
  FRecvBuffer bug fixed, it was freed, but never recreated, resulting in an AV

  Rev 1.21    2003.06.03 6:28:04 PM  czhower
  Made check for data virtual

  Rev 1.20    2003.06.03 3:43:24 PM  czhower
  Resolved InputBuffer inconsistency. Added new method and renamed old one.

  Rev 1.19    5/25/2003 03:56:04 AM  JPMugaas
  Updated for unit rename.

  Rev 1.18    2003.04.17 11:01:12 PM  czhower

    Rev 1.17    4/16/2003 3:29:30 PM  BGooijen
  minor change in ReadBuffer

    Rev 1.16    4/1/2003 7:54:24 PM  BGooijen
  ReadLn default terminator changed to LF

    Rev 1.15    3/27/2003 3:24:06 PM  BGooijen
  MaxLine* is now published

  Rev 1.14    2003.03.25 7:42:12 PM  czhower
  try finally to WriteStrings

    Rev 1.13    3/24/2003 11:01:36 PM  BGooijen
  WriteStrings is now buffered to increase speed

    Rev 1.12    3/19/2003 1:02:32 PM  BGooijen
  changed class function ConstructDefaultIOHandler a little (default parameter)

    Rev 1.11    3/13/2003 10:18:16 AM  BGooijen
  Server side fibers, bug fixes

    Rev 1.10    3/5/2003 11:03:06 PM  BGooijen
  Added Intercept here

    Rev 1.9    2/25/2003 11:02:12 PM  BGooijen
  InputBufferToStream now accepts a bytecount

  Rev 1.8    2003.02.25 1:36:00 AM  czhower

  Rev 1.7    12-28-2002 22:28:16  BGooijen
  removed warning, added initialization and finalization part.

  Rev 1.6    12-16-2002 20:43:28  BGooijen
  Added class function ConstructIOHandler(....), and removed some comments

  Rev 1.5    12-15-2002 23:02:38  BGooijen
  added SendBufferSize

  Rev 1.4    12-15-2002 20:50:32  BGooijen
  FSendBufferSize was not initialized

  Rev 1.3    12-14-2002 22:14:54  BGooijen
  improved method to detect timeouts in ReadLn.

  Rev 1.2    12/11/2002 04:09:28 AM  JPMugaas
  Updated for new API.

  Rev 1.1    2002.12.07 12:25:56 AM  czhower

  Rev 1.0    11/13/2002 08:44:50 AM  JPMugaas
}

unit IdIOHandler;

interface

{$I IdCompilerDefines.inc}

uses
  Classes,
  IdException,
  IdAntiFreezeBase, IdBuffer, IdBaseComponent, IdComponent, IdGlobal, IdExceptionCore,
  IdIntercept, IdResourceStringsCore, IdStream;

(*$HPPEMIT '#if defined(_VCL_ALIAS_RECORDS)' *)
(*$HPPEMIT '#if !defined(UNICODE)' *)
(*$HPPEMIT '#pragma alias "@Idiohandler@TIdIOHandler@SetPortA$qqri"="@Idiohandler@TIdIOHandler@SetPort$qqri"' *)
(*$HPPEMIT '#else' *)
(*$HPPEMIT '#pragma alias "@Idiohandler@TIdIOHandler@SetPortW$qqri"="@Idiohandler@TIdIOHandler@SetPort$qqri"' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '#endif' *)

const
  GRecvBufferSizeDefault = 32 * 1024;
  GSendBufferSizeDefault = 32 * 1024;
  IdMaxLineLengthDefault = 16 * 1024;
  // S.G. 6/4/2004: Maximum number of lines captured
  // S.G. 6/4/2004: Default to "unlimited"
  Id_IOHandler_MaxCapturedLines = -1;

type

  EIdIOHandler = class(EIdException);
  EIdIOHandlerRequiresLargeStream = class(EIdIOHandler);
  EIdIOHandlerStreamDataTooLarge = class(EIdIOHandler);

  TIdIOHandlerClass = class of TIdIOHandler;

  {
  How does this fit in in the hierarchy against TIdIOHandlerSocket
  Destination - Socket - otehr file descendats it

  TIdIOHandler should only implement an interface. No default functionality
  except very simple read/write functions such as ReadUInt32, etc. Functions
  that cannot really be optimized beyond their default implementations.

  Some default implementations offer basic non optmized implementations.

  Yes, I know this comment conflicts. Its being worked on.
  }
  TIdIOHandler = class(TIdComponent)
  private
    FLargeStream: Boolean;
  protected
    FClosedGracefully: Boolean;
    FConnectTimeout: Integer;
    FDestination: string;
    FHost: string;
    // IOHandlers typically receive more data than they need to complete each
    // request. They store this extra data in InputBuffer for future methods to
    // use. InputBuffer is what collects the input and keeps it if the current
    // method does not need all of it.
    //
    FInputBuffer: TIdBuffer;
    {$IFDEF USE_OBJECT_ARC}[Weak]{$ENDIF} FIntercept: TIdConnectionIntercept;
    FMaxCapturedLines: Integer;
    FMaxLineAction: TIdMaxLineAction;
    FMaxLineLength: Integer;
    FOpened: Boolean;
    FPort: Integer;
    FReadLnSplit: Boolean;
    FReadLnTimedOut: Boolean;
    FReadTimeOut: Integer;
//TODO:
    FRecvBufferSize: Integer;
    FSendBufferSize: Integer;

    FWriteBuffer: TIdBuffer;
    FWriteBufferThreshold: Integer;
    FDefStringEncoding : IIdTextEncoding;
    {$IFDEF STRING_IS_ANSI}
    FDefAnsiEncoding : IIdTextEncoding;
    {$ENDIF}
    procedure SetDefStringEncoding(const AEncoding : IIdTextEncoding);
    {$IFDEF STRING_IS_ANSI}
    procedure SetDefAnsiEncoding(const AEncoding: IIdTextEncoding);
    {$ENDIF}
    //
    procedure BufferRemoveNotify(ASender: TObject; ABytes: Integer);
    function GetDestination: string; virtual;
    procedure InitComponent; override;
    procedure InterceptReceive(var VBuffer: TIdBytes);
    {$IFNDEF USE_OBJECT_ARC}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ENDIF}
    procedure PerformCapture(const ADest: TObject; out VLineCount: Integer;
     const ADelim: string; AUsesDotTransparency: Boolean; AByteEncoding: IIdTextEncoding = nil
     {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
     ); virtual;
    procedure RaiseConnClosedGracefully;
    procedure SetDestination(const AValue: string); virtual;
    procedure SetHost(const AValue: string); virtual;
    procedure SetPort(AValue: Integer); virtual;
    procedure SetIntercept(AValue: TIdConnectionIntercept); virtual;
    // This is the main Read function which all other default implementations
    // use.
    function ReadFromSource(ARaiseExceptionIfDisconnected: Boolean = True;
     ATimeout: Integer = IdTimeoutDefault;
     ARaiseExceptionOnTimeout: Boolean = True): Integer;
    function ReadDataFromSource(var VBuffer: TIdBytes): Integer; virtual; abstract;
    function WriteDataToTarget(const ABuffer: TIdBytes; const AOffset, ALength: Integer): Integer; virtual; abstract;
    function SourceIsAvailable: Boolean; virtual; abstract;
    function CheckForError(ALastResult: Integer): Integer; virtual; abstract;
    procedure RaiseError(AError: Integer); virtual; abstract;
  public
    {$IFDEF WORKAROUND_INLINE_CONSTRUCTORS}
    constructor Create(AOwner: TComponent); reintroduce; overload;
    {$ENDIF}
    procedure AfterAccept; virtual;
    function Connected: Boolean; virtual;
    destructor Destroy; override;
    // CheckForDisconnect allows the implementation to check the status of the
    // connection at the request of the user or this base class.
    procedure CheckForDisconnect(ARaiseExceptionIfDisconnected: Boolean = True;
     AIgnoreBuffer: Boolean = False); virtual; abstract;
    // Does not wait or raise any exceptions. Just reads whatever data is
    // available (if any) into the buffer. Must NOT raise closure exceptions.
    // It is used to get avialable data, and check connection status. That is
    // it can set status flags about the connection.
    function CheckForDataOnSource(ATimeout: Integer = 0): Boolean; virtual;
    procedure Close; virtual;
    procedure CloseGracefully; virtual;
    class function MakeDefaultIOHandler(AOwner: TComponent = nil)
     : TIdIOHandler;
    class function MakeIOHandler(ABaseType: TIdIOHandlerClass;
     AOwner: TComponent = nil): TIdIOHandler;
    // Variant of MakeIOHandler() which returns nil if it cannot find a registered IOHandler
    class function TryMakeIOHandler(ABaseType: TIdIOHandlerClass;
     AOwner: TComponent = nil): TIdIOHandler;
    class procedure RegisterIOHandler;
    class procedure SetDefaultClass;
    function WaitFor(const AString: string; ARemoveFromBuffer: Boolean = True;
      AInclusive: Boolean = False; AByteEncoding: IIdTextEncoding = nil;
      ATimeout: Integer = IdTimeoutDefault
      {$IFDEF STRING_IS_ANSI}; AAnsiEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string;
    // This is different than WriteDirect. WriteDirect goes
    // directly to the network or next level. WriteBuffer allows for buffering
    // using WriteBuffers. This should be the only call to WriteDirect
    // unless the calls that bypass this are aware of WriteBuffering or are
    // intended to bypass it.
    procedure Write(const ABuffer: TIdBytes; const ALength: Integer = -1; const AOffset: Integer = 0); overload; virtual;
    // This is the main write function which all other default implementations
    // use. If default implementations are used, this must be implemented.
    procedure WriteDirect(const ABuffer: TIdBytes; const ALength: Integer = -1; const AOffset: Integer = 0);
    //
    procedure Open; virtual;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; virtual;
    //
    // Optimal Extra Methods
    //
    // These methods are based on the core methods. While they can be
    // overridden, they are so simple that it is rare a more optimal method can
    // be implemented. Because of this they are not overrideable.
    //
    //
    // Write Methods
    //
    // Only the ones that have a hope of being better optimized in descendants
    // have been marked virtual
    procedure Write(const AOut: string; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; virtual;
    procedure WriteLn(AEncoding: IIdTextEncoding = nil); overload;
    procedure WriteLn(const AOut: string; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; virtual;
    procedure WriteLnRFC(const AOut: string = ''; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); virtual;
    procedure Write(AValue: TStrings; AWriteLinesCount: Boolean = False;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; virtual;
    procedure Write(AValue: Byte); overload;
    procedure Write(AValue: Char; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;

    // for iOS64, Delphi's Longint and LongWord are 64bit, so we can't rely on
    // Write(Longint) and ReadLongint() being 32bit anymore, for instance when
    // sending/reading a TStream with LargeStream=False.  So adding new (U)IntX
    // methods and deprecating the old ones...
    //
    procedure Write(AValue: Int16; AConvert: Boolean = True); overload;
    procedure Write(AValue: UInt16; AConvert: Boolean = True); overload;
    procedure Write(AValue: Int32; AConvert: Boolean = True); overload;
    procedure Write(AValue: UInt32; AConvert: Boolean = True); overload;
    procedure Write(AValue: Int64; AConvert: Boolean = True); overload;
    procedure Write(AValue: TIdUInt64; AConvert: Boolean = True); overload;
    //

    procedure Write(AStream: TStream; ASize: TIdStreamSize = 0;
      AWriteByteCount: Boolean = False); overload; virtual;
    procedure WriteRFCStrings(AStrings: TStrings; AWriteTerminator: Boolean = True;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
      );
    // Not overloaded because it does not have a unique type for source
    // and could be easily unresolvable with future additions
    function WriteFile(const AFile: String; AEnableTransferFile: Boolean = False): Int64; virtual;
    //
    // Read methods
    //
    function AllData(AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; virtual;
    function InputLn(const AMask: string = ''; AEcho: Boolean = True;
      ATabWidth: Integer = 8; AMaxLineLength: Integer = -1;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; AAnsiEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; virtual;
    // Capture
    // Not virtual because each calls PerformCapture which is virtual
    procedure Capture(ADest: TStream; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; // .Net overload
    procedure Capture(ADest: TStream; ADelim: string;
      AUsesDotTransparency: Boolean = True; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Capture(ADest: TStream; out VLineCount: Integer;
      const ADelim: string = '.'; AUsesDotTransparency: Boolean = True;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Capture(ADest: TStrings; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload; // .Net overload
    procedure Capture(ADest: TStrings; const ADelim: string;
      AUsesDotTransparency: Boolean = True; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    procedure Capture(ADest: TStrings; out VLineCount: Integer;
      const ADelim: string = '.'; AUsesDotTransparency: Boolean = True;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ); overload;
    //
    // Read___
    // Cannot overload, compiler cannot overload on return values
    //
    procedure ReadBytes(var VBuffer: TIdBytes; AByteCount: Integer; AAppend: Boolean = True); virtual;
    // ReadLn
    function ReadLn(AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; overload; // .Net overload
    function ReadLn(ATerminator: string; AByteEncoding: IIdTextEncoding
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; overload;
    function ReadLn(ATerminator: string; ATimeout: Integer = IdTimeoutDefault;
      AMaxLineLength: Integer = -1; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; overload; virtual;
    //RLebeau: added for RFC 822 retrieves
    function ReadLnRFC(var VMsgEnd: Boolean; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; overload;
    function ReadLnRFC(var VMsgEnd: Boolean; const ALineTerminator: string;
      const ADelim: string = '.'; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; overload;
    function ReadLnWait(AFailCount: Integer = MaxInt;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string; virtual;
    // Added for retrieving lines over 16K long}
    function ReadLnSplit(var AWasSplit: Boolean; ATerminator: string = LF;
      ATimeout: Integer = IdTimeoutDefault; AMaxLineLength: Integer = -1;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string;
    // Read - Simple Types
    function ReadChar(AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): Char;
    function ReadByte: Byte;
    function ReadString(ABytes: Integer; AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string;

    // for iOS64, Delphi's Longint and LongWord are changed to 64bit, so we can't
    // rely on Write(Longint) and ReadLongint() being 32bit anymore, for instance
    // when sending/reading a TStream with LargeStream=False.  So adding new (U)IntX
    // methods and deprecating the old ones...
    //
    function ReadInt16(AConvert: Boolean = True): Int16;
    function ReadUInt16(AConvert: Boolean = True): UInt16;
    function ReadInt32(AConvert: Boolean = True): Int32;
    function ReadUInt32(AConvert: Boolean = True): UInt32;
    function ReadInt64(AConvert: Boolean = True): Int64;
    function ReadUInt64(AConvert: Boolean = True): TIdUInt64;
    //
    function ReadSmallInt(AConvert: Boolean = True): Int16; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ReadInt16()'{$ENDIF};{$ENDIF}
    function ReadWord(AConvert: Boolean = True): UInt16; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ReadUInt16()'{$ENDIF};{$ENDIF}
    function ReadLongInt(AConvert: Boolean = True): Int32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ReadInt32()'{$ENDIF};{$ENDIF}
    function ReadLongWord(AConvert: Boolean = True): UInt32; {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use ReadUInt32()'{$ENDIF};{$ENDIF}
    //

    procedure ReadStream(AStream: TStream; AByteCount: TIdStreamSize = -1;
     AReadUntilDisconnect: Boolean = False); virtual;
    procedure ReadStrings(ADest: TStrings; AReadLinesCount: Integer = -1;
      AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      );
    //
    procedure Discard(AByteCount: Int64);
    procedure DiscardAll;
    //
    // WriteBuffering Methods
    //
    procedure WriteBufferCancel; virtual;
    procedure WriteBufferClear; virtual;
    procedure WriteBufferClose; virtual;
    procedure WriteBufferFlush; overload; //.Net overload
    procedure WriteBufferFlush(AByteCount: Integer); overload; virtual;
    procedure WriteBufferOpen; overload; //.Net overload
    procedure WriteBufferOpen(AThreshold: Integer); overload; virtual;
    function WriteBufferingActive: Boolean;
    //
    // InputBuffer Methods
    //
    function InputBufferIsEmpty: Boolean;
    //
    // These two are direct access and do no reading of connection
    procedure InputBufferToStream(AStream: TStream; AByteCount: Integer = -1);
    function InputBufferAsString(AByteEncoding: IIdTextEncoding = nil
      {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
      ): string;
    //
    // Properties
    //
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property ClosedGracefully: Boolean read FClosedGracefully;
    // TODO: Need to name this consistent. Originally no access was allowed,
    // but new model requires it for writing. Will decide after next set
    // of changes are complete what to do with Buffer prop.
    //
    // Is used by SuperCore
    property InputBuffer: TIdBuffer read FInputBuffer;
    //currently an option, as LargeFile support changes the data format
    property LargeStream: Boolean read FLargeStream write FLargeStream;
    property MaxCapturedLines: Integer read FMaxCapturedLines write FMaxCapturedLines default Id_IOHandler_MaxCapturedLines;
    property Opened: Boolean read FOpened;
    property ReadTimeout: Integer read FReadTimeOut write FReadTimeOut default IdTimeoutDefault;
    property ReadLnTimedout: Boolean read FReadLnTimedout ;
    property WriteBufferThreshold: Integer read FWriteBufferThreshold;
    property DefStringEncoding : IIdTextEncoding read FDefStringEncoding write SetDefStringEncoding;
    {$IFDEF STRING_IS_ANSI}
    property DefAnsiEncoding : IIdTextEncoding read FDefAnsiEncoding write SetDefAnsiEncoding;
    {$ENDIF}
    //
    // Events
    //
    property OnWork;
    property OnWorkBegin;
    property OnWorkEnd;
  published
    property Destination: string read GetDestination write SetDestination;
    property Host: string read FHost write SetHost;
    property Intercept: TIdConnectionIntercept read FIntercept write SetIntercept;
    property MaxLineLength: Integer read FMaxLineLength write FMaxLineLength default IdMaxLineLengthDefault;
    property MaxLineAction: TIdMaxLineAction read FMaxLineAction write FMaxLineAction;
    property Port: Integer read FPort write SetPort;
    // RecvBufferSize is used by some methods that read large amounts of data.
    // RecvBufferSize is the amount of data that will be requested at each read
    // cycle. RecvBuffer is used to receive then send to the Intercepts, after
    // that it goes to InputBuffer
    property RecvBufferSize: Integer read FRecvBufferSize write FRecvBufferSize
     default GRecvBufferSizeDefault;
    // SendBufferSize is used by some methods that have to break apart large
    // amounts of data into smaller pieces. This is the buffer size of the
    // chunks that it will create and use.
    property SendBufferSize: Integer read FSendBufferSize write FSendBufferSize
     default GSendBufferSizeDefault;
  end;

implementation

uses
  //facilitate inlining only.
  {$IFDEF DOTNET}
    {$IFDEF USE_INLINE}
  System.IO,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WIN32_OR_WIN64}
  Windows,
  {$ENDIF}
  {$IFDEF USE_VCL_POSIX}
    {$IFDEF DARWIN}
  Macapi.CoreServices,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF HAS_UNIT_Generics_Collections}
  System.Generics.Collections,
  {$ENDIF}
  IdStack, IdStackConsts, IdResourceStrings,
  SysUtils;

type
  {$IFDEF HAS_GENERICS_TList}
  TIdIOHandlerClassList = TList<TIdIOHandlerClass>;
  {$ELSE}
  // TODO: flesh out to match TList<TIdIOHandlerClass> for non-Generics compilers
  TIdIOHandlerClassList = TList;
  {$ENDIF}

var
  GIOHandlerClassDefault: TIdIOHandlerClass = nil;
  GIOHandlerClassList: TIdIOHandlerClassList = nil;

{$IFDEF DCC}
  {$IFNDEF VCL_7_OR_ABOVE}
    // RLebeau 5/13/2015: The Write(Int64) and ReadInt64() methods produce an
    // "Internal error URW533" compiler error in Delphi 5, and an "Internal
    // error URW699" compiler error in Delphi 6, so need to use some workarounds
    // for those versions...
    {$DEFINE AVOID_URW_ERRORS}
  {$ENDIF}
{$ENDIF}

{ TIdIOHandler }

{$IFDEF WORKAROUND_INLINE_CONSTRUCTORS}
constructor TIdIOHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{$ENDIF}

procedure TIdIOHandler.Close;
//do not do FInputBuffer.Clear; here.
//it breaks reading when remote connection does a disconnect
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  try
    LIntercept := Intercept;
    if LIntercept <> nil then begin
      LIntercept.Disconnect;
    end;
  finally
    FOpened := False;
    WriteBufferClear;
  end;
end;

destructor TIdIOHandler.Destroy;
begin
  Close;
  FreeAndNil(FInputBuffer);
  FreeAndNil(FWriteBuffer);
  inherited Destroy;
end;

procedure TIdIOHandler.AfterAccept;
begin
  //
end;

procedure TIdIOHandler.Open;
begin
  FOpened := False;
  FClosedGracefully := False;
  WriteBufferClear;
  FInputBuffer.Clear;
  FOpened := True;
end;

// under ARC, all weak references to a freed object get nil'ed automatically
{$IFNDEF USE_OBJECT_ARC}
procedure TIdIOHandler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FIntercept) then begin
    FIntercept := nil;
  end;
  inherited Notification(AComponent, OPeration);
end;
{$ENDIF}

procedure TIdIOHandler.SetIntercept(AValue: TIdConnectionIntercept);
begin
  {$IFDEF USE_OBJECT_ARC}
  // under ARC, all weak references to a freed object get nil'ed automatically
  FIntercept := AValue;
  {$ELSE}
  if FIntercept <> AValue then begin
    // remove self from the Intercept's free notification list
    if Assigned(FIntercept) then begin
      FIntercept.RemoveFreeNotification(Self);
    end;
    FIntercept := AValue;
    // add self to the Intercept's free notification list
    if Assigned(AValue) then begin
      AValue.FreeNotification(Self);
    end;
  end;
  {$ENDIF}
end;

class procedure TIdIOHandler.SetDefaultClass;
begin
  GIOHandlerClassDefault := Self;
  RegisterIOHandler;
end;

procedure TIdIOHandler.SetDefStringEncoding(const AEncoding: IIdTextEncoding);
var
  LEncoding: IIdTextEncoding;
begin
  if FDefStringEncoding <> AEncoding then
  begin
    LEncoding := AEncoding;
    EnsureEncoding(LEncoding);
    FDefStringEncoding := LEncoding;
  end;
end;

{$IFDEF STRING_IS_ANSI}
procedure TIdIOHandler.SetDefAnsiEncoding(const AEncoding: IIdTextEncoding);
var
  LEncoding: IIdTextEncoding;
begin
  if FDefAnsiEncoding <> AEncoding then
  begin
    LEncoding := AEncoding;
    EnsureEncoding(LEncoding, encOSDefault);
    FDefAnsiEncoding := LEncoding;
  end;
end;
{$ENDIF}

class function TIdIOHandler.MakeDefaultIOHandler(AOwner: TComponent = nil): TIdIOHandler;
begin
  Result := GIOHandlerClassDefault.Create(AOwner);
end;

class procedure TIdIOHandler.RegisterIOHandler;
begin
  if GIOHandlerClassList = nil then begin
    GIOHandlerClassList := TIdIOHandlerClassList.Create;
  end;
  {$IFNDEF DOTNET_EXCLUDE}
  //TODO: Reenable this. Dot net wont allow class references as objects
  // Use an array?
  if GIOHandlerClassList.IndexOf(Self) = -1 then begin
    GIOHandlerClassList.Add(Self);
  end;
  {$ENDIF}
end;

{
  Creates an IOHandler of type ABaseType, or descendant.
}
class function TIdIOHandler.MakeIOHandler(ABaseType: TIdIOHandlerClass;
  AOwner: TComponent = nil): TIdIOHandler;
begin
  Result := TryMakeIOHandler(ABaseType, AOwner);
  if not Assigned(Result) then begin
    raise EIdException.CreateFmt(RSIOHandlerTypeNotInstalled, [ABaseType.ClassName]);
  end;
end;

class function TIdIOHandler.TryMakeIOHandler(ABaseType: TIdIOHandlerClass;
  AOwner: TComponent = nil): TIdIOHandler;
var
  i: Integer;
begin
  if GIOHandlerClassList <> nil then begin
    for i := GIOHandlerClassList.Count - 1 downto 0 do begin
      if TIdIOHandlerClass(GIOHandlerClassList[i]).InheritsFrom(ABaseType) then begin
        Result := TIdIOHandlerClass(GIOHandlerClassList[i]).Create;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TIdIOHandler.GetDestination: string;
begin
  Result := FDestination;
end;

procedure TIdIOHandler.SetDestination(const AValue: string);
begin
  FDestination := AValue;
end;

procedure TIdIOHandler.BufferRemoveNotify(ASender: TObject; ABytes: Integer);
begin
  DoWork(wmRead, ABytes);
end;

procedure TIdIOHandler.WriteBufferOpen(AThreshold: Integer);
begin
  if FWriteBuffer <> nil then begin
    FWriteBuffer.Clear;
  end else begin
    FWriteBuffer := TIdBuffer.Create;
  end;
  FWriteBufferThreshold := AThreshold;
end;

procedure TIdIOHandler.WriteBufferClose;
begin
  try
    WriteBufferFlush;
  finally FreeAndNil(FWriteBuffer); end;
end;

procedure TIdIOHandler.WriteBufferFlush(AByteCount: Integer);
var
  LBytes: TIdBytes;
begin
  if FWriteBuffer <> nil then begin
    if FWriteBuffer.Size > 0 then begin
      FWriteBuffer.ExtractToBytes(LBytes, AByteCount);
      WriteDirect(LBytes);
    end;
  end;
end;

procedure TIdIOHandler.WriteBufferClear;
begin
  if FWriteBuffer <> nil then begin
    FWriteBuffer.Clear;
  end;
end;

procedure TIdIOHandler.WriteBufferCancel;
begin
  WriteBufferClear;
  WriteBufferClose;
end;

procedure TIdIOHandler.Write(const AOut: string; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  if AOut <> '' then begin
    AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
    {$IFDEF STRING_IS_ANSI}
    ASrcEncoding := iif(ASrcEncoding, FDefAnsiEncoding, encOSDefault);
    {$ENDIF}
    Write(
      ToBytes(AOut, -1, 1, AByteEncoding
        {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
        )
      );
  end;
end;

procedure TIdIOHandler.Write(AValue: Byte);
begin
  Write(ToBytes(AValue));
end;

procedure TIdIOHandler.Write(AValue: Char; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ASrcEncoding := iif(ASrcEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  Write(
    ToBytes(AValue, AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
      )
    );
end;

procedure TIdIOHandler.Write(AValue: UInt32; AConvert: Boolean = True);
begin
  if AConvert then begin
    AValue := GStack.HostToNetwork(AValue);
  end;
  Write(ToBytes(AValue));
end;

procedure TIdIOHandler.Write(AValue: Int32; AConvert: Boolean = True);
begin
  if AConvert then begin
    AValue := Int32(GStack.HostToNetwork(UInt32(AValue)));
  end;
  Write(ToBytes(AValue));
end;

{$IFDEF HAS_UInt64}
  {$IFDEF BROKEN_UInt64_HPPEMIT}
    {$DEFINE HAS_TIdUInt64_QuadPart}
  {$ENDIF}
{$ELSE}
  {$IFNDEF HAS_QWord}
    {$DEFINE HAS_TIdUInt64_QuadPart}
  {$ENDIF}
{$ENDIF}

procedure TIdIOHandler.Write(AValue: Int64; AConvert: Boolean = True);
{$IFDEF AVOID_URW_ERRORS}
var
  h: Int64;
{$ELSE}
  {$IFDEF HAS_TIdUInt64_QuadPart}
var
  h: TIdUInt64;
  {$ENDIF}
{$ENDIF}
begin
  if AConvert then begin
    {$IFDEF AVOID_URW_ERRORS}
    // assigning to a local variable to avoid an "Internal error URW533" compiler
    // error in Delphi 5, and an "Internal error URW699" compiler error in Delphi
    // 6.  Later versions seem OK without it...
    h := GStack.HostToNetwork(UInt64(AValue));
    AValue := h;
    {$ELSE}
      {$IFDEF HAS_TIdUInt64_QuadPart}
    // assigning to a local variable if UInt64 is not a native type, or if using
    // a C++Builder version that has problems with UInt64 parameters...
    h.QuadPart := UInt64(AValue);
    h := GStack.HostToNetwork(h);
    AValue := Int64(h.QuadPart);
      {$ELSE}
    AValue := Int64(GStack.HostToNetwork(UInt64(AValue)));
      {$ENDIF}
    {$ENDIF}
  end;
  Write(ToBytes(AValue));
end;

procedure TIdIOHandler.Write(AValue: TIdUInt64; AConvert: Boolean = True);
begin
  if AConvert then begin
    AValue := GStack.HostToNetwork(AValue);
  end;
  Write(ToBytes(AValue));
end;

procedure TIdIOHandler.Write(AValue: TStrings; AWriteLinesCount: Boolean = False;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  i: Integer;
  LBufferingStarted: Boolean;
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ASrcEncoding := iif(ASrcEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  LBufferingStarted := not WriteBufferingActive;
  if LBufferingStarted then begin
    WriteBufferOpen;
  end;
  try
    if AWriteLinesCount then begin
      Write(AValue.Count);
    end;
    for i := 0 to AValue.Count - 1 do begin
      WriteLn(AValue.Strings[i], AByteEncoding
        {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
        );
    end;
    if LBufferingStarted then begin
      WriteBufferClose;
    end;
  except
    if LBufferingStarted then begin
      WriteBufferCancel;
    end;
    raise;
  end;
end;

procedure TIdIOHandler.Write(AValue: UInt16; AConvert: Boolean = True);
begin
  if AConvert then begin
    AValue := GStack.HostToNetwork(AValue);
  end;
  Write(ToBytes(AValue));
end;

procedure TIdIOHandler.Write(AValue: Int16; AConvert: Boolean = True);
begin
  if AConvert then begin
    AValue := Int16(GStack.HostToNetwork(UInt16(AValue)));
  end;
  Write(ToBytes(AValue));
end;

function TIdIOHandler.ReadString(ABytes: Integer; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LBytes: TIdBytes;
begin
  if ABytes > 0 then begin
    ReadBytes(LBytes, ABytes, False);
    AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
    {$IFDEF STRING_IS_ANSI}
    ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
    {$ENDIF}
    Result := BytesToString(LBytes, 0, ABytes, AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
      );
  end else begin
    Result := '';
  end;
end;

procedure TIdIOHandler.ReadStrings(ADest: TStrings; AReadLinesCount: Integer = -1;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  i: Integer;
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  if AReadLinesCount < 0 then begin
    AReadLinesCount := ReadInt32;
  end;
  for i := 0 to AReadLinesCount - 1 do begin
    ADest.Add(ReadLn(AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
      ));
  end;
end;

function TIdIOHandler.ReadUInt16(AConvert: Boolean = True): UInt16;
var
  LBytes: TIdBytes;
begin
  ReadBytes(LBytes, SizeOf(UInt16), False);
  Result := BytesToUInt16(LBytes);
  if AConvert then begin
    Result := GStack.NetworkToHost(Result);
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdIOHandler.ReadWord(AConvert: Boolean = True): UInt16;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadUInt16(AConvert);
end;

function TIdIOHandler.ReadInt16(AConvert: Boolean = True): Int16;
var
  LBytes: TIdBytes;
begin
  ReadBytes(LBytes, SizeOf(Int16), False);
  Result := BytesToInt16(LBytes);
  if AConvert then begin
    Result := Int16(GStack.NetworkToHost(UInt16(Result)));
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdIOHandler.ReadSmallInt(AConvert: Boolean = True): Int16;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadInt16(AConvert);
end;

function TIdIOHandler.ReadChar(AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): Char;
var
  I, J, NumChars, NumBytes: Integer;
  LBytes: TIdBytes;
  {$IFDEF DOTNET}
  LChars: array[0..1] of Char;
  {$ELSE}
  LChars: TIdWideChars;
    {$IFDEF STRING_IS_ANSI}
  LWTmp: TIdUnicodeString;
  LATmp: TIdBytes;
    {$ENDIF}
  {$ENDIF}
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  // 2 Chars to handle UTF-16 surrogates
  NumBytes := AByteEncoding.GetMaxByteCount(2);
  SetLength(LBytes, NumBytes);
  {$IFNDEF DOTNET}
  SetLength(LChars, 2);
  {$ENDIF}
  NumChars := 0;
  if NumBytes > 0 then
  begin
    for I := 1 to NumBytes do
    begin
      LBytes[I-1] := ReadByte;
      NumChars := AByteEncoding.GetChars(LBytes, 0, I, LChars, 0);
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
  // it somewhere for the the next ReadChar() call to retreive.  Just
  // raise an error for now.  Users will have to update their code to
  // read surrogates differently...
  Assert(NumChars = 1);
  Result := LChars[0];
  {$ELSE}
  // RLebeau: since we can only return an AnsiChar here, let's convert
  // the decoded characters, surrogates and all, into their Ansi
  // representation. This will have the same problem as above if the
  // conversion results in a multibyte character sequence...
  SetString(LWTmp, PWideChar(LChars), NumChars);
  LATmp := ADestEncoding.GetBytes(LWTmp); // convert to Ansi
  Assert(Length(LATmp) = 1);
  Result := Char(LATmp[0]);
  {$ENDIF}
end;

function TIdIOHandler.ReadByte: Byte;
var
  LBytes: TIdBytes;
begin
  ReadBytes(LBytes, 1, False);
  Result := LBytes[0];
end;

function TIdIOHandler.ReadInt32(AConvert: Boolean): Int32;
var
  LBytes: TIdBytes;
begin
  ReadBytes(LBytes, SizeOf(Int32), False);
  Result := BytesToInt32(LBytes);
  if AConvert then begin
    Result := Int32(GStack.NetworkToHost(UInt32(Result)));
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdIOHandler.ReadLongInt(AConvert: Boolean): Int32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadInt32(AConvert);
end;

function TIdIOHandler.ReadInt64(AConvert: boolean): Int64;
var
  LBytes: TIdBytes;
  {$IFDEF AVOID_URW_ERRORS}
  h: Int64;
  {$ELSE}
    {$IFDEF HAS_TIdUInt64_QuadPart}
  h: TIdUInt64;
    {$ENDIF}
  {$ENDIF}
begin
  ReadBytes(LBytes, SizeOf(Int64), False);
  Result := BytesToInt64(LBytes);
  if AConvert then begin
    {$IFDEF AVOID_URW_ERRORS}
    // assigning to a local variable to avoid an "Internal error URW533" compiler
    // error in Delphi 5, and an "Internal error URW699" compiler error in Delphi
    // 6.  Later versions seem OK without it...
    h := GStack.NetworkToHost(UInt64(Result));
    Result := h;
    {$ELSE}
      {$IFDEF HAS_TIdUInt64_QuadPart}
    // assigning to a local variable if UInt64 is not a native type, or if using
    // a C++Builder version that has problems with UInt64 parameters...
    h.QuadPart := UInt64(Result);
    h := GStack.NetworkToHost(h);
    Result := Int64(h.QuadPart);
      {$ELSE}
    Result := Int64(GStack.NetworkToHost(UInt64(Result)));
      {$ENDIF}
    {$ENDIF}
  end;
end;

function TIdIOHandler.ReadUInt64(AConvert: boolean): TIdUInt64;
var
  LBytes: TIdBytes;
begin
  ReadBytes(LBytes, SizeOf(TIdUInt64), False);
  Result := BytesToUInt64(LBytes);
  if AConvert then begin
    Result := GStack.NetworkToHost(Result);
  end;
end;

function TIdIOHandler.ReadUInt32(AConvert: Boolean): UInt32;
var
  LBytes: TIdBytes;
begin
  ReadBytes(LBytes, SizeOf(UInt32), False);
  Result := BytesToUInt32(LBytes);
  if AConvert then begin
    Result := GStack.NetworkToHost(Result);
  end;
end;

{$I IdDeprecatedImplBugOff.inc}
function TIdIOHandler.ReadLongWord(AConvert: Boolean): UInt32;
{$I IdDeprecatedImplBugOn.inc}
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadUInt32(AConvert);
end;

function TIdIOHandler.ReadLn(AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadLn(LF, IdTimeoutDefault, -1, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

function TIdIOHandler.ReadLn(ATerminator: string; AByteEncoding: IIdTextEncoding
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadLn(ATerminator, IdTimeoutDefault, -1, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

function TIdIOHandler.ReadLn(ATerminator: string; ATimeout: Integer = IdTimeoutDefault;
  AMaxLineLength: Integer = -1; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LInputBufferSize: Integer;
  LStartPos: Integer;
  LTermPos: Integer;
  LReadLnStartTime: TIdTicks;
  LTerm, LResult: TIdBytes;
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  if AMaxLineLength < 0 then begin
    AMaxLineLength := MaxLineLength;
  end;
  // User may pass '' if they need to pass arguments beyond the first.
  if ATerminator = '' then begin
    ATerminator := LF;
  end;
  // TODO: encountered an email that was using charset "cp1026", which encodes
  // a LF character to byte $25 instead of $0A (and decodes byte $0A to character
  // #$8E instead of #$A).  To account for that, don't encoding the LF using the
  // specified encoding anymore, force the encoding to what it should be.  But
  // what if UTF-16 is being used?
  {
  if ATerminator = LF then begin
    LTerm := ToBytes(Byte($0A));
  end else begin
    LTerm := ToBytes(ATerminator, AByteEncoding
      {$IFDEF STRING_IS_ANSI, ADestEncoding{$ENDIF
      );
  end;
  }
  LTerm := ToBytes(ATerminator, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
  FReadLnSplit := False;
  FReadLnTimedOut := False;
  LTermPos := -1;
  LStartPos := 0;
  LReadLnStartTime := Ticks64;
  repeat
    LInputBufferSize := FInputBuffer.Size;
    if LInputBufferSize > 0 then begin
      if LStartPos < LInputBufferSize then begin
        LTermPos := FInputBuffer.IndexOf(LTerm, LStartPos);
      end else begin
        LTermPos := -1;
      end;
      LStartPos := IndyMax(LInputBufferSize-(Length(LTerm)-1), 0);
    end;
    // if the line length is limited and terminator is found after the limit or not found and the limit is exceeded
    if (AMaxLineLength > 0) and ((LTermPos > AMaxLineLength) or ((LTermPos = -1) and (LStartPos > AMaxLineLength))) then begin
      if MaxLineAction = maException then begin
        raise EIdReadLnMaxLineLengthExceeded.Create(RSReadLnMaxLineLengthExceeded);
      end;
      // RLebeau: WARNING - if the line is using multibyte character sequences
      // and a sequence staddles the AMaxLineLength boundary, this will chop
      // the sequence, producing invalid data!
      FReadLnSplit := True;
      Result := FInputBuffer.ExtractToString(AMaxLineLength, AByteEncoding
        {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
        );
      Exit;
    end
    // ReadFromSource blocks - do not call unless we need to
    else if LTermPos = -1 then begin
      // ReadLn needs to call this as data may exist in the buffer, but no EOL yet disconnected
      CheckForDisconnect(True, True);
      // Can only return -1 if timeout
      FReadLnTimedOut := ReadFromSource(True, ATimeout, False) = -1;
      if (not FReadLnTimedOut) and (ATimeout >= 0) then begin
        if GetElapsedTicks(LReadLnStartTime) >= UInt32(ATimeout) then begin
          FReadLnTimedOut := True;
        end;
      end;
      if FReadLnTimedOut then begin
        Result := '';
        Exit;
      end;
    end;
  until LTermPos > -1;
  // Extract actual data
  {
  IMPORTANT!!!

   When encoding from UTF8 to Unicode or ASCII, you will not always get the same
   number of bytes that you input so you may have to recalculate LTermPos since
   that was based on the number of bytes in the input stream.  If do not do this,
   you will probably get an incorrect result or a range check error since the
   string is shorter then the original buffer position.

   JPM
   }
  // RLebeau 11/19/08: this is no longer needed as the terminator is encoded to raw bytes now ...
  {
  Result := FInputBuffer.Extract(LTermPos + Length(ATerminator), AEncoding);
  LTermPos := IndyMin(LTermPos, Length(Result));
  if (ATerminator = LF) and (LTermPos > 0) then begin
    if Result[LTermPos] = CR then begin
      Dec(LTermPos);
    end;
  end;
  SetLength(Result, LTermPos);
  }
  FInputBuffer.ExtractToBytes(LResult, LTermPos + Length(LTerm));
  if (ATerminator = LF) and (LTermPos > 0) then begin
    if LResult[LTermPos-1] = Ord(CR) then begin
      Dec(LTermPos);
    end;
  end;
  Result := BytesToString(LResult, 0, LTermPos, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

function TIdIOHandler.ReadLnRFC(var VMsgEnd: Boolean;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := ReadLnRFC(VMsgEnd, LF, '.', AByteEncoding   {do not localize}
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

function TIdIOHandler.ReadLnRFC(var VMsgEnd: Boolean; const ALineTerminator: string;
  const ADelim: String = '.'; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
begin
  Result := ReadLn(ALineTerminator, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
  // Do not use ATerminator since always ends with . (standard)
  if Result = ADelim then
  begin
    VMsgEnd := True;
    Exit;
  end;
  if TextStartsWith(Result, '..') then begin {do not localize}
    Delete(Result, 1, 1);
  end;
  VMsgEnd := False;
end;

function TIdIOHandler.ReadLnSplit(var AWasSplit: Boolean; ATerminator: string = LF;
  ATimeout: Integer = IdTimeoutDefault; AMaxLineLength: Integer = -1;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  FOldAction: TIdMaxLineAction;
begin
  FOldAction := MaxLineAction;
  MaxLineAction := maSplit;
  try
    Result := ReadLn(ATerminator, ATimeout, AMaxLineLength, AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
      );
    AWasSplit := FReadLnSplit;
  finally
    MaxLineAction := FOldAction;
  end;
end;

function TIdIOHandler.ReadLnWait(AFailCount: Integer = MaxInt;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LAttempts: Integer;
begin
  // MtW: this is mostly used when empty lines could be sent.
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  Result := '';
  LAttempts := 0;
  while LAttempts < AFailCount do
  begin
    Result := Trim(ReadLn(AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
      ));
    if Length(Result) > 0 then begin
      Exit;
    end;
    if ReadLnTimedOut then begin
      raise EIdReadTimeout.Create(RSReadTimeout);
    end;
    Inc(LAttempts);
  end;
  raise EIdReadLnWaitMaxAttemptsExceeded.Create(RSReadLnWaitMaxAttemptsExceeded);
end;

function TIdIOHandler.ReadFromSource(ARaiseExceptionIfDisconnected: Boolean;
  ATimeout: Integer; ARaiseExceptionOnTimeout: Boolean): Integer;
var
  LByteCount: Integer;
  LLastError: Integer;
  LBuffer: TIdBytes;
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  if ATimeout = IdTimeoutDefault then begin
    // MtW: check for 0 too, for compatibility
    if (ReadTimeout = IdTimeoutDefault) or (ReadTimeout = 0) then begin
      ATimeout := IdTimeoutInfinite;
    end else begin
      ATimeout := ReadTimeout;
    end;
  end;
  Result := 0;
  // Check here as this side may have closed the socket
  CheckForDisconnect(ARaiseExceptionIfDisconnected);
  if SourceIsAvailable then begin
    repeat
      LByteCount := 0;
      if Readable(ATimeout) then begin
        if Opened then begin
          // No need to call AntiFreeze, the Readable does that.
          if SourceIsAvailable then begin
            // TODO: Whey are we reallocating LBuffer every time? This
            // should be a one time operation per connection.

            // RLebeau: because the Intercept does not allow the buffer
            // size to be specified, and the Intercept could potentially
            // resize the buffer...

            SetLength(LBuffer, RecvBufferSize);
            try
              LByteCount := ReadDataFromSource(LBuffer);
              if LByteCount > 0 then begin
                SetLength(LBuffer, LByteCount);

                LIntercept := Intercept;
                if LIntercept <> nil then begin
                  LIntercept.Receive(LBuffer);
                  {$IFDEF USE_OBJECT_ARC}LIntercept := nil;{$ENDIF}
                  LByteCount := Length(LBuffer);
                end;

                // Pass through LBuffer first so it can go through Intercept
                //TODO: If not intercept, we can skip this step
                InputBuffer.Write(LBuffer);
              end;
            finally
              LBuffer := nil;
            end;
          end
          else if ARaiseExceptionIfDisconnected then begin
            raise EIdClosedSocket.Create(RSStatusDisconnected);
          end;
        end
        else if ARaiseExceptionIfDisconnected then begin
          raise EIdNotConnected.Create(RSNotConnected);
        end;
        if LByteCount < 0 then
        begin
          LLastError := CheckForError(LByteCount);
          if LLastError = Id_WSAETIMEDOUT then begin
            // Timeout
            if ARaiseExceptionOnTimeout then begin
              raise EIdReadTimeout.Create(RSReadTimeout);
            end;
            Result := -1;
            Break;
          end;
          FClosedGracefully := True;
          Close;
          // Do not raise unless all data has been read by the user
          if InputBufferIsEmpty and ARaiseExceptionIfDisconnected then begin
            RaiseError(LLastError);
          end;
          LByteCount := 0;
        end
        else if LByteCount = 0 then begin
          FClosedGracefully := True;
        end;
        // Check here as other side may have closed connection
        CheckForDisconnect(ARaiseExceptionIfDisconnected);
        Result := LByteCount;
      end else begin
        // Timeout
        if ARaiseExceptionOnTimeout then begin
          raise EIdReadTimeout.Create(RSReadTimeout);
        end;
        Result := -1;
        Break;
      end;
    until (LByteCount <> 0) or (not SourceIsAvailable);
  end
  else if ARaiseExceptionIfDisconnected then begin
    raise EIdNotConnected.Create(RSNotConnected);
  end;
end;

function TIdIOHandler.CheckForDataOnSource(ATimeout: Integer = 0): Boolean;
var
  LPrevSize: Integer;
begin
  Result := False;
  // RLebeau - Connected() might read data into the InputBuffer, thus
  // leaving no data for ReadFromSource() to receive a second time,
  // causing a result of False when it should be True instead.  So we
  // save the current size of the InputBuffer before calling Connected()
  // and then compare it afterwards....
  LPrevSize := InputBuffer.Size;
  if Connected then begin
    // return whether at least 1 byte was received
    Result := (InputBuffer.Size > LPrevSize) or (ReadFromSource(False, ATimeout, False) > 0);
  end;
end;

procedure TIdIOHandler.Write(AStream: TStream; ASize: TIdStreamSize = 0;
  AWriteByteCount: Boolean = FALSE);
var
  LBuffer: TIdBytes;
  LStreamPos: TIdStreamSize;
  LBufSize: Integer;
  // LBufferingStarted: Boolean;
begin
  // TODO: when AWriteByteCount is false, don't calculate the Size, just keep
  // sending data until the end of stream is reached.  This way, we can send
  // streams that are not able to report accurate Size values...
  
  if ASize < 0 then begin //"-1" All from current position
    LStreamPos := AStream.Position;
    ASize := AStream.Size - LStreamPos;
    //todo is this step required?
    AStream.Position := LStreamPos;
  end
  else if ASize = 0 then begin //"0" ALL
    ASize := AStream.Size;
    AStream.Position := 0;
  end;
  //else ">0" number of bytes

  // RLebeau 3/19/2006: DO NOT ENABLE WRITE BUFFERING IN THIS METHOD!
  //
  // When sending large streams, especially with LargeStream enabled,
  // this can easily cause "Out of Memory" errors.  It is the caller's
  // responsibility to enable/disable write buffering as needed before
  // calling one of the Write() methods.
  //
  // Also, forcing write buffering in this method is having major
  // impacts on TIdFTP, TIdFTPServer, and TIdHTTPServer.

  if AWriteByteCount then begin
    if LargeStream then begin
      Write(Int64(ASize));
    end else begin
      {$IFDEF STREAM_SIZE_64}
      if ASize > High(Integer) then begin
        raise EIdIOHandlerRequiresLargeStream.Create(RSRequiresLargeStream);
      end;
      {$ENDIF}
      Write(Int32(ASize));
    end;
  end;

  BeginWork(wmWrite, ASize);
  try
    SetLength(LBuffer, FSendBufferSize);
    while ASize > 0 do begin
      LBufSize := IndyMin(ASize, Length(LBuffer));
      // Do not use ReadBuffer. Some source streams are real time and will not
      // return as much data as we request. Kind of like recv()
      // NOTE: We use .Size - size must be supported even if real time
      LBufSize := TIdStreamHelper.ReadBytes(AStream, LBuffer, LBufSize);
      if LBufSize <= 0 then begin
        raise EIdNoDataToRead.Create(RSIdNoDataToRead);
      end;
      Write(LBuffer, LBufSize);
      // RLebeau: DoWork() is called in WriteDirect()
      //DoWork(wmWrite, LBufSize);
      Dec(ASize, LBufSize);
    end;
  finally
    EndWork(wmWrite);
    LBuffer := nil;
  end;
end;

procedure TIdIOHandler.ReadBytes(var VBuffer: TIdBytes; AByteCount: Integer; AAppend: Boolean = True);
begin
  Assert(FInputBuffer<>nil);
  if AByteCount > 0 then begin
    // Read from stack until we have enough data
    while FInputBuffer.Size < AByteCount do begin
      // RLebeau: in case the other party disconnects
      // after all of the bytes were transmitted ok.
      // No need to throw an exception just yet...
      if ReadFromSource(False) > 0 then begin
        if FInputBuffer.Size >= AByteCount then begin
          Break; // we have enough data now
        end;
      end;
      CheckForDisconnect(True, True);
    end;
    FInputBuffer.ExtractToBytes(VBuffer, AByteCount, AAppend);
  end else if AByteCount < 0 then begin
    // Return whatever data is currently in the InputBuffer
    if InputBufferIsEmpty then begin
      // Read whatever data is currently on the stack
      ReadFromSource(False, ReadTimeout, False);
      CheckForDisconnect(True, True);
    end;
    FInputBuffer.ExtractToBytes(VBuffer, -1, AAppend);
  end;
end;

procedure TIdIOHandler.WriteLn(AEncoding: IIdTextEncoding = nil);
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  {$IFNDEF VCL_6_OR_ABOVE}
  // RLebeau: in Delphi 5, explicitly specifying the nil value for the third
  // parameter causes a "There is no overloaded version of 'WriteLn' that can
  // be called with these arguments" compiler error.  Must be a compiler bug,
  // because it compiles fine in Delphi 6.  The parameter value is nil by default
  // anyway, so we don't really need to specify it here at all, but I'm documenting
  // this so we know for future reference...
  //
  WriteLn('', AEncoding);
  {$ELSE}
  WriteLn('', AEncoding{$IFDEF STRING_IS_ANSI}, nil{$ENDIF});
  {$ENDIF}
end;

procedure TIdIOHandler.WriteLn(const AOut: string;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  // TODO: RLebeau 1/2/2015: encountered an email that was using charset "cp1026",
  // which encodes a LF character to byte $25 instead of $0A (and decodes
  // byte $0A to character #$8E instead of #$A).  To account for that, don't
  // encoding the CRLF using the specified encoding anymore, force the encoding
  // to what it should be...
  //
  // But, what to do if the target encoding is UTF-16?
  {
  Write(AOut, AByteEncoding{$IFDEF STRING_IS_ANSI, ASrcEncoding{$ENDIF);
  Write(EOL, Indy8BitEncoding{$IFDEF STRING_IS_ANSI, Indy8BitEncoding{$ENDIF);
  }

  // Do as one write so it only makes one call to network
  Write(AOut + EOL, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.WriteLnRFC(const AOut: string = '';
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
begin
  if TextStartsWith(AOut, '.') then begin {do not localize}
    WriteLn('.' + AOut, AByteEncoding     {do not localize}
      {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
      );
  end else begin
    WriteLn(AOut, AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
      );
  end;
end;

function TIdIOHandler.Readable(AMSec: Integer): Boolean;
begin
  // In case descendant does not override this or other methods but implements the higher level
  // methods
  Result := False;
end;

procedure TIdIOHandler.SetHost(const AValue: string);
begin
  FHost := AValue;
end;

procedure TIdIOHandler.SetPort(AValue: Integer);
begin
  FPort := AValue;
end;

function TIdIOHandler.Connected: Boolean;
begin
  CheckForDisconnect(False);
  Result :=
   (
     (
       // Set when closed properly. Reflects actual socket state.
       (not ClosedGracefully)
       // Created on Open. Prior to Open ClosedGracefully is still false.
       and (FInputBuffer <> nil)
     )
     // Buffer must be empty. Even if closed, we are "connected" if we still have
     // data
     or (not InputBufferIsEmpty)
   )
   and Opened;
end;

// TODO: move this into IdGlobal.pas
procedure AdjustStreamSize(const AStream: TStream; const ASize: TIdStreamSize);
var
  LStreamPos: TIdStreamSize;
begin
  LStreamPos := AStream.Position;
  AStream.Size := ASize;
  // Must reset to original value in cases where size changes position
  if AStream.Position <> LStreamPos then begin
    AStream.Position := LStreamPos;
  end;
end;

procedure TIdIOHandler.ReadStream(AStream: TStream; AByteCount: TIdStreamSize;
  AReadUntilDisconnect: Boolean);
var
  i: Integer;
  LBuf: TIdBytes;
  LByteCount, LPos: TIdStreamSize;
  {$IFNDEF STREAM_SIZE_64}
  LTmp: Int64;
  {$ENDIF}
const
  cSizeUnknown = -1;
begin
  if (AByteCount = cSizeUnknown) and (not AReadUntilDisconnect) then begin
    // Read size from connection
    if LargeStream then begin
      {$IFDEF STREAM_SIZE_64}
      LByteCount := ReadInt64;
      {$ELSE}
      LTmp := ReadInt64;
      if LTmp > MaxInt then begin
        raise EIdIOHandlerStreamDataTooLarge.Create(RSDataTooLarge);
      end;
      LByteCount := TIdStreamSize(LTmp);
      {$ENDIF}
    end else begin
      LByteCount := ReadInt32;
    end;
  end else begin
    LByteCount := AByteCount;
  end;

  // Presize stream if we know the size - this reduces memory/disk allocations to one time.
  // TODO: need to add an option for this. user might not want to presize here, eg for reading
  // int64 files, or when filling a manually-sized file using multiple threads.
  if (AStream <> nil) and (LByteCount > -1) then begin
    LPos := AStream.Position;
    if (High(TIdStreamSize) - LPos) < LByteCount then begin
      raise EIdIOHandlerStreamDataTooLarge.Create(RSDataTooLarge);
    end;
    AdjustStreamSize(AStream, LPos + LByteCount);
  end;

  if (LByteCount <= cSizeUnknown) and (not AReadUntilDisconnect) then begin
    AReadUntilDisconnect := True;
  end;

  if AReadUntilDisconnect then begin
    BeginWork(wmRead);
  end else begin
    BeginWork(wmRead, LByteCount);
  end;

  try
    // If data already exists in the buffer, write it out first.
    // should this loop for all data in buffer up to workcount? not just one block?
    if FInputBuffer.Size > 0 then begin
      if AReadUntilDisconnect then begin
        i := FInputBuffer.Size;
      end else begin
        i := IndyMin(FInputBuffer.Size, LByteCount);
        Dec(LByteCount, i);
      end;
      if AStream <> nil then begin
        FInputBuffer.ExtractToStream(AStream, i);
      end else begin
        FInputBuffer.Remove(i);
      end;
    end;

    // RLebeau - don't call Connected() here!  ReadBytes() already
    // does that internally. Calling Connected() here can cause an
    // EIdConnClosedGracefully exception that breaks the loop
    // prematurely and thus leave unread bytes in the InputBuffer.
    // Let the loop catch the exception before exiting...

    SetLength(LBuf, RecvBufferSize); // preallocate the buffer
    repeat
      if AReadUntilDisconnect then begin
        i := Length(LBuf);
      end else begin
        i := IndyMin(LByteCount, Length(LBuf));
        if i < 1 then begin
          Break;
        end;
      end;

      //TODO: Improve this - dont like the use of the exception handler
      //DONE -oAPR: Dont use a string, use a memory buffer or better yet the buffer itself.

      //TODO: Don't use ReadBytes() here. It is just a waste of memory. Use
      //ReadFromSource() directly to populate the InputBuffer (ReadBytes()
      //would have done that anyway) and then use InputBuffer.ExtractToStream()
      //to copy directly into the TStream. We don't really need another memory
      //buffer here...

      try
        try
          ReadBytes(LBuf, i, False);
        except
          on E: Exception do begin
            // RLebeau - ReadFromSource() inside of ReadBytes()
            // could have filled the InputBuffer with more bytes
            // than actually requested, so don't extract too
            // many bytes here...
            i := IndyMin(i, FInputBuffer.Size);
            FInputBuffer.ExtractToBytes(LBuf, i, False);
            if AReadUntilDisconnect then begin
              if E is EIdConnClosedGracefully then begin
                Exit;
              end
              else if E is EIdSocketError then begin
                case EIdSocketError(E).LastError of
                  Id_WSAESHUTDOWN, Id_WSAECONNABORTED, Id_WSAECONNRESET: begin
                    Exit;
                  end;
                end;
              end;
            end;
            raise;
          end;
        end;
        TIdAntiFreezeBase.DoProcess;
      finally
        if i > 0 then begin
          if AStream <> nil then begin
            TIdStreamHelper.Write(AStream, LBuf, i);
          end;
          if not AReadUntilDisconnect then begin
            Dec(LByteCount, i);
          end;
        end;
      end;
    until False;
  finally
    EndWork(wmRead);
    if AStream <> nil then begin
      if AStream.Size > AStream.Position then begin
        AStream.Size := AStream.Position;
      end;
    end;
    LBuf := nil;
  end;
end;

procedure TIdIOHandler.Discard(AByteCount: Int64);
var
  LSize: Integer;
begin
  Assert(AByteCount >= 0);
  if AByteCount > 0 then
  begin
    BeginWork(wmRead, AByteCount);
    try
      repeat
        LSize := iif(AByteCount < MaxInt, Integer(AByteCount), MaxInt);
        LSize := IndyMin(LSize, FInputBuffer.Size);
        if LSize > 0 then begin
          FInputBuffer.Remove(LSize);
          Dec(AByteCount, LSize);
          if AByteCount < 1 then begin
            Break;
          end;
        end;
        // RLebeau: in case the other party disconnects
        // after all of the bytes were transmitted ok.
        // No need to throw an exception just yet...
        if ReadFromSource(False) < 1 then begin
          CheckForDisconnect(True, True);
        end;
      until False;
    finally
      EndWork(wmRead);
    end;
  end;
end;

procedure TIdIOHandler.DiscardAll;
begin
  BeginWork(wmRead);
  try
    // If data already exists in the buffer, discard it first.
    FInputBuffer.Clear;
    // RLebeau - don't call Connected() here!  ReadBytes() already
    // does that internally. Calling Connected() here can cause an
    // EIdConnClosedGracefully exception that breaks the loop
    // prematurely and thus leave unread bytes in the InputBuffer.
    // Let the loop catch the exception before exiting...
    repeat
      //TODO: Improve this - dont like the use of the exception handler
      try
        if ReadFromSource(False) > 0 then begin
          FInputBuffer.Clear;
        end else begin;
          CheckForDisconnect(True, True);
        end;
      except
        on E: Exception do begin
          // RLebeau - ReadFromSource() could have filled the
          // InputBuffer with more bytes...
          FInputBuffer.Clear;
          if E is EIdConnClosedGracefully then begin
            Break;
          end else begin
            raise;
          end;
        end;
      end;
      TIdAntiFreezeBase.DoProcess;
    until False;
  finally
    EndWork(wmRead);
  end;
end;

procedure TIdIOHandler.RaiseConnClosedGracefully;
begin
  (* ************************************************************* //
  ------ If you receive an exception here, please read. ----------

  If this is a SERVER
  -------------------
  The client has disconnected the socket normally and this exception is used to notify the
  server handling code. This exception is normal and will only happen from within the IDE, not
  while your program is running as an EXE. If you do not want to see this, add this exception
  or EIdSilentException to the IDE options as exceptions not to break on.

  From the IDE just hit F9 again and Indy will catch and handle the exception.

  Please see the FAQ and help file for possible further information.
  The FAQ is at http://www.nevrona.com/Indy/FAQ.html

  If this is a CLIENT
  -------------------
  The server side of this connection has disconnected normaly but your client has attempted
  to read or write to the connection. You should trap this error using a try..except.
  Please see the help file for possible further information.

  // ************************************************************* *)
  raise EIdConnClosedGracefully.Create(RSConnectionClosedGracefully);
end;

function TIdIOHandler.InputBufferAsString(AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  Result := FInputBuffer.ExtractToString(FInputBuffer.Size, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

function TIdIOHandler.AllData(AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LBytes: Integer;
begin
  Result := '';
  BeginWork(wmRead);
  try
    if Connected then
    begin
      try
        try
          repeat
            LBytes := ReadFromSource(False, 250, False);
          until LBytes = 0; // -1 on timeout
        finally
          if not InputBufferIsEmpty then begin
            Result := InputBufferAsString(AByteEncoding
              {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
              );
          end;
        end;
      except end;
    end;
  finally
    EndWork(wmRead);
  end;
end;

procedure TIdIOHandler.PerformCapture(const ADest: TObject;
  out VLineCount: Integer; const ADelim: string;
  AUsesDotTransparency: Boolean; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  s: string;
  LStream: TStream;
  LStrings: TStrings;
begin
  VLineCount := 0;

  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ADestEncoding := iif(ADestEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}

  LStream := nil;
  LStrings := nil;

  if ADest is TStrings then begin
    LStrings := TStrings(ADest);
  end
  else if ADest is TStream then begin
    LStream := TStream(ADest);
  end
  else begin
    raise EIdObjectTypeNotSupported.Create(RSObjectTypeNotSupported);
  end;

  BeginWork(wmRead);
  try
    repeat
      s := ReadLn(AByteEncoding
        {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
        );
      if s = ADelim then begin
        Exit;
      end;
      // S.G. 6/4/2004: All the consumers to protect themselves against memory allocation attacks
      if FMaxCapturedLines > 0 then  begin
        if VLineCount > FMaxCapturedLines then begin
          raise EIdMaxCaptureLineExceeded.Create(RSMaximumNumberOfCaptureLineExceeded);
        end;
      end;
      // For RFC retrieves that use dot transparency
      // No length check necessary, if only one byte it will be byte x + #0.
      if AUsesDotTransparency then begin
        if TextStartsWith(s, '..') then begin
          Delete(s, 1, 1);
        end;
      end;
      // Write to output
      Inc(VLineCount);
      if LStrings <> nil then begin
        LStrings.Add(s);
      end
      else if LStream <> nil then begin
        WriteStringToStream(LStream, s+EOL, AByteEncoding
          {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
          );
      end;
    until False;
  finally
    EndWork(wmRead);
  end;
end;

function TIdIOHandler.InputLn(const AMask: String = ''; AEcho: Boolean = True;
  ATabWidth: Integer = 8; AMaxLineLength: Integer = -1;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; AAnsiEncoding: IIdTextEncoding = nil{$ENDIF}
  ): String;
var
  i: Integer;
  LChar: Char;
  LTmp: string;
begin
  Result := '';
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  AAnsiEncoding := iif(AAnsiEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  if AMaxLineLength < 0 then begin
    AMaxLineLength := MaxLineLength;
  end;
  repeat
    LChar := ReadChar(AByteEncoding
      {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
      );
    i := Length(Result);
    if i <= AMaxLineLength then begin
      case LChar of
        BACKSPACE:
          begin
            if i > 0 then begin
              SetLength(Result, i - 1);
              if AEcho then begin
                Write(BACKSPACE + ' ' + BACKSPACE, AByteEncoding
                  {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
                  );
              end;
            end;
          end;
        TAB:
          begin
            if ATabWidth > 0 then begin
              i := ATabWidth - (i mod ATabWidth);
              LTmp := StringOfChar(' ', i);
              Result := Result + LTmp;
              if AEcho then begin
                Write(LTmp, AByteEncoding
                  {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
                  );
              end;
            end else begin
              Result := Result + LChar;
              if AEcho then begin
                Write(LChar, AByteEncoding
                  {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
                  );
              end;
            end;
          end;
        LF: ;
        CR: ;
        #27: ; //ESC - currently not supported
      else
        Result := Result + LChar;
        if AEcho then begin
          if Length(AMask) = 0 then begin
            Write(LChar, AByteEncoding
              {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
              );
          end else begin
            Write(AMask, AByteEncoding
              {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
              );
          end;
        end;
      end;
    end;
  until LChar = LF;
  // Remove CR trail
  i := Length(Result);
  while (i > 0) and CharIsInSet(Result, i, EOL) do begin
    Dec(i);
  end;
  SetLength(Result, i);
  if AEcho then begin
    WriteLn(AByteEncoding);
  end;
end;

//TODO: Add a time out (default to infinite) and event to pass data
//TODO: Add a max size argument as well.
//TODO: Add a case insensitive option
function TIdIOHandler.WaitFor(const AString: string; ARemoveFromBuffer: Boolean = True;
  AInclusive: Boolean = False; AByteEncoding: IIdTextEncoding = nil;
  ATimeout: Integer = IdTimeoutDefault
  {$IFDEF STRING_IS_ANSI}; AAnsiEncoding: IIdTextEncoding = nil{$ENDIF}
  ): string;
var
  LBytes: TIdBytes;
  LPos: Integer;
begin
  Result := '';
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  AAnsiEncoding := iif(AAnsiEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  LBytes := ToBytes(AString, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
    );
  LPos := 0;
  repeat
    LPos := InputBuffer.IndexOf(LBytes, LPos);
    if LPos <> -1 then begin
      if ARemoveFromBuffer and AInclusive then begin
        Result := InputBuffer.ExtractToString(LPos+Length(LBytes), AByteEncoding
          {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
          );
      end else begin
        Result := InputBuffer.ExtractToString(LPos, AByteEncoding
          {$IFDEF STRING_IS_ANSI}, AAnsiEncoding{$ENDIF}
          );
        if ARemoveFromBuffer then begin
          InputBuffer.Remove(Length(LBytes));
        end;
        if AInclusive then begin
          Result := Result + AString;
        end;
      end;
      Exit;
    end;
    LPos := IndyMax(0, InputBuffer.Size - (Length(LBytes)-1));
    ReadFromSource(True, ATimeout, True);
  until False;
end;

procedure TIdIOHandler.Capture(ADest: TStream; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Capture(ADest, '.', True, AByteEncoding     {do not localize}
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.Capture(ADest: TStream; out VLineCount: Integer;
  const ADelim: string = '.'; AUsesDotTransparency: Boolean = True;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  PerformCapture(ADest, VLineCount, ADelim, AUsesDotTransparency, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.Capture(ADest: TStream; ADelim: string;
  AUsesDotTransparency: Boolean = True; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  LLineCount: Integer;
begin
  PerformCapture(ADest, LLineCount, '.', AUsesDotTransparency, AByteEncoding   {do not localize}
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.Capture(ADest: TStrings; out VLineCount: Integer;
  const ADelim: string = '.'; AUsesDotTransparency: Boolean = True;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  PerformCapture(ADest, VLineCount, ADelim, AUsesDotTransparency, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.Capture(ADest: TStrings; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  LLineCount: Integer;
begin
  PerformCapture(ADest, LLineCount, '.', True, AByteEncoding    {do not localize}
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.Capture(ADest: TStrings; const ADelim: string;
  AUsesDotTransparency: Boolean = True; AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ADestEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  LLineCount: Integer;
begin
  PerformCapture(ADest, LLineCount, ADelim, AUsesDotTransparency, AByteEncoding
    {$IFDEF STRING_IS_ANSI}, ADestEncoding{$ENDIF}
    );
end;

procedure TIdIOHandler.InputBufferToStream(AStream: TStream; AByteCount: Integer = -1);
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  FInputBuffer.ExtractToStream(AStream, AByteCount);
end;

function TIdIOHandler.InputBufferIsEmpty: Boolean;
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := FInputBuffer.Size = 0;
end;

procedure TIdIOHandler.Write(const ABuffer: TIdBytes; const ALength: Integer = -1;
  const AOffset: Integer = 0);
var
  LLength: Integer;
begin
  LLength := IndyLength(ABuffer, ALength, AOffset);
  if LLength > 0 then begin
    if FWriteBuffer = nil then begin
      WriteDirect(ABuffer, LLength, AOffset);
    end else begin
      // Write Buffering is enabled
      FWriteBuffer.Write(ABuffer, LLength, AOffset);
      if (FWriteBuffer.Size >= WriteBufferThreshold) and (WriteBufferThreshold > 0) then begin
        repeat
          WriteBufferFlush(WriteBufferThreshold);
        until FWriteBuffer.Size < WriteBufferThreshold;
      end;
    end;
  end;
end;

procedure TIdIOHandler.WriteRFCStrings(AStrings: TStrings; AWriteTerminator: Boolean = True;
  AByteEncoding: IIdTextEncoding = nil
  {$IFDEF STRING_IS_ANSI}; ASrcEncoding: IIdTextEncoding = nil{$ENDIF}
  );
var
  i: Integer;
begin
  AByteEncoding := iif(AByteEncoding, FDefStringEncoding);
  {$IFDEF STRING_IS_ANSI}
  ASrcEncoding := iif(ASrcEncoding, FDefAnsiEncoding, encOSDefault);
  {$ENDIF}
  for i := 0 to AStrings.Count - 1 do begin
    WriteLnRFC(AStrings[i], AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
      );
  end;
  if AWriteTerminator then begin
    WriteLn('.', AByteEncoding
      {$IFDEF STRING_IS_ANSI}, ASrcEncoding{$ENDIF}
      );
  end;
end;

function TIdIOHandler.WriteFile(const AFile: String; AEnableTransferFile: Boolean): Int64;
var
  LStream: TStream;
  {$IFDEF WIN32_OR_WIN64}
  LOldErrorMode : Integer;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF WIN32_OR_WIN64}
  LOldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
  {$ENDIF}
    if not FileExists(AFile) then begin
      raise EIdFileNotFound.CreateFmt(RSFileNotFound, [AFile]);
    end;
    LStream := TIdReadFileExclusiveStream.Create(AFile);
    try
      Write(LStream);
      Result := LStream.Size;
    finally
      FreeAndNil(LStream);
    end;
  {$IFDEF WIN32_OR_WIN64}
  finally
    SetErrorMode(LOldErrorMode)
  end;
  {$ENDIF}
end;

function TIdIOHandler.WriteBufferingActive: Boolean;
{$IFDEF USE_CLASSINLINE}inline;{$ENDIF}
begin
  Result := FWriteBuffer <> nil;
end;

procedure TIdIOHandler.CloseGracefully;
begin
  FClosedGracefully := True
end;

procedure TIdIOHandler.InterceptReceive(var VBuffer: TIdBytes);
var
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  LIntercept := Intercept;
  if LIntercept <> nil then begin
    LIntercept.Receive(VBuffer);
  end;
end;

procedure TIdIOHandler.InitComponent;
begin
  inherited InitComponent;
  FRecvBufferSize := GRecvBufferSizeDefault;
  FSendBufferSize := GSendBufferSizeDefault;
  FMaxLineLength := IdMaxLineLengthDefault;
  FMaxCapturedLines := Id_IOHandler_MaxCapturedLines;
  FLargeStream := False;
  FReadTimeOut := IdTimeoutDefault;
  FInputBuffer := TIdBuffer.Create(BufferRemoveNotify);
  FDefStringEncoding := IndyTextEncoding_ASCII;
  {$IFDEF STRING_IS_ANSI}
  FDefAnsiEncoding := IndyTextEncoding_OSDefault;
  {$ENDIF}
end;

procedure TIdIOHandler.WriteBufferFlush;
begin
  WriteBufferFlush(-1);
end;

procedure TIdIOHandler.WriteBufferOpen;
begin
  WriteBufferOpen(-1);
end;

procedure TIdIOHandler.WriteDirect(const ABuffer: TIdBytes; const ALength: Integer = -1;
  const AOffset: Integer = 0);
var
  LTemp: TIdBytes;
  LPos: Integer;
  LSize: Integer;
  LByteCount: Integer;
  LLastError: Integer;
  // under ARC, convert a weak reference to a strong reference before working with it
  LIntercept: TIdConnectionIntercept;
begin
  // Check if disconnected
  CheckForDisconnect(True, True);

  LIntercept := Intercept;
  if LIntercept <> nil then begin
    // TODO: pass offset/size parameters to the Intercept
    // so that a copy is no longer needed here
    LTemp := ToBytes(ABuffer, ALength, AOffset);
    LIntercept.Send(LTemp);
    {$IFDEF USE_OBJECT_ARC}LIntercept := nil;{$ENDIF}
    LSize := Length(LTemp);
    LPos := 0;
  end else begin
    LTemp := ABuffer;
    LSize := IndyLength(LTemp, ALength, AOffset);
    LPos := AOffset;
  end;
  while LSize > 0 do
  begin
    LByteCount := WriteDataToTarget(LTemp, LPos, LSize);
    if LByteCount < 0 then
    begin
      LLastError := CheckForError(LByteCount);
      if LLastError <> Id_WSAETIMEDOUT then begin
        FClosedGracefully := True;
        Close;
      end;
      RaiseError(LLastError);
    end;
    // TODO - Have a AntiFreeze param which allows the send to be split up so that process
    // can be called more. Maybe a prop of the connection, MaxSendSize?
    TIdAntiFreezeBase.DoProcess(False);
    if LByteCount = 0 then begin
      FClosedGracefully := True;
    end;
    // Check if other side disconnected
    CheckForDisconnect;
    DoWork(wmWrite, LByteCount);
    Inc(LPos, LByteCount);
    Dec(LSize, LByteCount);
  end;
end;

initialization

finalization
  FreeAndNil(GIOHandlerClassList)
end.
