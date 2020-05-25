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
  Rev 1.3    09/06/2004 09:52:34  CCostelloe
  Kylix 3 patch

  Rev 1.2    6/4/2004 5:12:56 PM  SGrobety
  added EIdMaxCaptureLineExceeded

  Rev 1.1    2/10/2004 7:41:50 PM  JPMugaas
  I had to move EWrapperException down to the system package because
  IdStackDotNET was using it and that would drage IdExceptionCore into the
  package.  Borland changed some behavior so the warning is now an error.

  Rev 1.0    2004.02.03 4:19:48 PM  czhower
  Rename

  Rev 1.15    11/4/2003 10:26:58 PM  DSiders
  Added exceptions moved from IdIOHandler.pas and IdTCPConnection.pas.

  Rev 1.14    2003.10.16 11:24:00 AM  czhower
  Added IfAssigned

  Rev 1.13    2003.10.11 5:47:58 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.12    10/3/2003 11:38:36 PM  GGrieve
  Add EIdWrapperException

  Rev 1.11    9/29/2003 02:56:28 PM  JPMugaas
  Added comment about why IdException.Create is virtual.

  Rev 1.10    9/24/2003 11:42:50 PM  JPMugaas
  Minor changes to help compile under NET

  Rev 1.9    2003.09.19 10:10:02 PM  czhower
  IfTrue, IfFalse

  Rev 1.8    2003.09.19 11:54:28 AM  czhower
  -Completed more features necessary for servers
  -Fixed some bugs

  Rev 1.7    2003.07.17 4:57:04 PM  czhower
  Added new exception type so it can be added to debugger list of ignored
  exceptions.

  Rev 1.6    7/1/2003 8:33:02 PM  BGooijen
  Added EIdFibersNotSupported

  Rev 1.5    2003.06.05 10:08:50 AM  czhower
  Extended reply mechanisms to the exception handling. Only base and RFC
  completed, handing off to J Peter.

  Rev 1.4    5/14/2003 2:59:58 PM  BGooijen
  Added exception for transparant proxy

  Rev 1.3    2003.04.14 10:54:06 AM  czhower
  Fiber specific exceptions

  Rev 1.2    4/2/2003 7:18:38 PM  BGooijen
  Added EIdHttpProxyError

  Rev 1.1    1/17/2003 05:06:46 PM  JPMugaas
  Exceptions for scheduler string.

  Rev 1.0    11/13/2002 08:44:10 AM  JPMugaas
}

unit IdExceptionCore;

interface
{$I IdCompilerDefines.inc}
//needed to put FCP into Delphi mode
uses
  IdException, IdStack;

type
  // IdFiber Exceptions
  EIdFiber = class(EIdException);
  EIdFiberFinished = class(EIdFiber);
  EIdFibersNotSupported = class(EIdFiber);

  EIdAlreadyConnected = class(EIdException);

  // EIdClosedSocket is raised if .Disconnect has been called and an operation is attempted
  // or Connect has not been called
  EIdClosedSocket = class(EIdException);
  EIdResponseError = class(EIdException);
  EIdReadTimeout = class(EIdException);
  EIdAcceptTimeout = class(EIdException);
  EIdReadLnMaxLineLengthExceeded = class(EIdException);
  EIdReadLnWaitMaxAttemptsExceeded = class(EIdException);

  // TIdTCPConnection exceptions
  EIdPortRequired = class(EIdException);
  EIdHostRequired = class(EIdException);
  EIdTCPConnectionError = class(EIdException);
  EIdObjectTypeNotSupported = class(EIdTCPConnectionError);
  EIdInterceptPropIsNil = class(EIdTCPConnectionError);
  EIdInterceptPropInvalid = class(EIdTCPConnectionError);
  EIdIOHandlerPropInvalid = class(EIdTCPConnectionError);
  EIdNoDataToRead = class(EIdTCPConnectionError);
  EIdFileNotFound = class(EIdTCPConnectionError);

  EIdNotConnected = class(EIdException);

  EInvalidSyslogMessage = class(EIdException);
  EIdSSLProtocolReplyError = class(EIdException);
  EIdConnectTimeout = class(EIdException);
  EIdConnectException = class(EIdException);

  EIdTransparentProxyCantBind = class(EIdException);

  EIdHttpProxyError = class(EIdException);

  EIdSocksError = class(EIdException);
  EIdSocksRequestFailed = class(EIdSocksError);
  EIdSocksRequestServerFailed = class(EIdSocksError);
  EIdSocksRequestIdentFailed = class(EIdSocksError);
  EIdSocksUnknownError = class(EIdSocksError);
  EIdSocksServerRespondError = class(EIdSocksError);
  EIdSocksAuthMethodError = class(EIdSocksError);
  EIdSocksAuthError = class(EIdSocksError);
  EIdSocksServerGeneralError = class(EIdSocksError);
  EIdSocksServerPermissionError = class (EIdSocksError);
  EIdSocksServerNetUnreachableError = class (EIdSocksError);
  EIdSocksServerHostUnreachableError = class (EIdSocksError);
  EIdSocksServerConnectionRefusedError = class (EIdSocksError);
  EIdSocksServerTTLExpiredError = class (EIdSocksError);
  EIdSocksServerCommandError = class (EIdSocksError);
  EIdSocksServerAddressError = class (EIdSocksError);

  //IdIMAP4 Exception
  EIdConnectionStateError = class(EIdException);

  // THE EDnsResolverError is used so the resolver can repond to only resolver execeptions.
  EIdDnsResolverError = Class(EIdException);

  {Socket exceptions}
  EIdInvalidSocket = class(EIdException);

  EIdThreadMgrError = class(EIdException);
  EIdThreadClassNotSpecified = class(EIdThreadMgrError);

  {TIdTrivial FTP Exception }
  EIdTFTPException               = class(EIdException);
  EIdTFTPFileNotFound            = class(EIdTFTPException);
  EIdTFTPAccessViolation         = class(EIdTFTPException);
  EIdTFTPAllocationExceeded      = class(EIdTFTPException);
  EIdTFTPIllegalOperation        = class(EIdTFTPException);
  EIdTFTPUnknownTransferID       = class(EIdTFTPException);
  EIdTFTPFileAlreadyExists       = class(EIdTFTPException);
  EIdTFTPNoSuchUser              = class(EIdTFTPException);
  EIdTFTPOptionNegotiationFailed = class(EIdTFTPException);  // RFC 1782

  {Icmp exceptions}
  EIdIcmpException = class(EIdException);

  EIdSetSizeExceeded = class(EIdException);

  {IdMessage and things use this}
  EIdMessageException = class(EIdException);

  //scheduler exception
  EIdSchedulerException = class(EIdException);
  EIdSchedulerMaxThreadsExceeded = class(EIdSchedulerException);

  { IdIOHandler }
  EIdMaxCaptureLineExceeded = class(EIdException); // S.G. 6/4/2004: triggered when a capture command exceeds the maximum number of line allowed

implementation

end.
