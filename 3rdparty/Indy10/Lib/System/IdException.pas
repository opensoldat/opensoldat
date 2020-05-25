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
  Rev 1.5    09/06/2004 09:52:52  CCostelloe
  Kylix 3 patch

  Rev 1.4    2004.04.18 4:38:24 PM  czhower
  EIdExceptionBase created

  Rev 1.3    2004.03.07 11:45:22 AM  czhower
  Flushbuffer fix + other minor ones found

  Rev 1.2    2/10/2004 7:33:24 PM  JPMugaas
  I had to move the wrapper exception here for DotNET stack because Borland's
  update 1 does not permit unlisted units from being put into a package.  That
  now would report an error and I didn't want to move IdExceptionCore into the
  System package.

  Rev 1.1    2004.02.03 3:15:52 PM  czhower
  Updates to move to System.

  Rev 1.0    2004.02.03 2:36:00 PM  czhower
  Move
}

unit IdException;

interface

{$I IdCompilerDefines.inc}

uses
  SysUtils;

type
  // EIdExceptionBase is the base class which extends Exception. It is separate from EIdException
  // to allow other users of Indy to use EIdExceptionBase while still being able to separate from
  // EIdException.
  EIdException = class(Exception)
  public
    {
    The constructor must be virtual for Delphi NET if you want to call it with class methods.
    Otherwise, it will not compile in that IDE. Also it's overloaded so that it doesn't close
    the other methods declared by the DotNet exception (particularly InnerException constructors)
    }
    constructor Create(const AMsg: string); overload; virtual;
    class procedure Toss(const AMsg: string); {$IFDEF HAS_DEPRECATED}deprecated{$IFDEF HAS_DEPRECATED_MSG} 'Use raise instead'{$ENDIF};{$ENDIF}
  end;

  TClassIdException = class of EIdException;

  // You can add EIdSilentException to the list of ignored exceptions to reduce debugger "trapping"
  // of "normal" exceptions
  EIdSilentException = class(EIdException);

  // EIdConnClosedGracefully is raised when remote side closes connection normally
  EIdConnClosedGracefully = class(EIdSilentException);

  {$IFDEF DOTNET}
  // This class used in DotNet. Under windows/linux, all errors that come out the
  // indy layer descend from IdException (actually not all errors in theory, but
  // certainly all errors in practice)
  // With DotNet, the socket library itself may raise various exceptions. If the
  // exception is a socket exception, then Indy will map this to an EIdSocketError.
  // Otherwise Indy will raise an EIdWrapperException. In this case, the original
  // exception will be available using the InnerException member
  EIdWrapperException = class (EIdException);
  {$ENDIF}
  
  //used for index out of range
  {CH EIdRangeException = class(EIdException); }

  // Other shared exceptions
  EIdSocketHandleError = class(EIdException);
  {$IFDEF UNIX}
  EIdNonBlockingNotSupported = class(EIdException);
  {$ENDIF}
  EIdPackageSizeTooBig = class(EIdSocketHandleError);
  EIdNotAllBytesSent = class (EIdSocketHandleError);
  EIdCouldNotBindSocket = class (EIdSocketHandleError);
  EIdCanNotBindPortInRange = class (EIdSocketHandleError);
  EIdInvalidPortRange = class(EIdSocketHandleError);
  EIdCannotSetIPVersionWhenConnected = class(EIdSocketHandleError);
  {CH EIdInvalidIPAddress = class(EIdSocketHandleError); }

implementation

{ EIdException }

constructor EIdException.Create(const AMsg : String);
begin
  inherited Create(AMsg);
end;

{$I IdDeprecatedImplBugOff.inc}
class procedure EIdException.Toss(const AMsg: string);
{$I IdDeprecatedImplBugOn.inc}
begin
  raise Create(AMsg);
end;

end.
