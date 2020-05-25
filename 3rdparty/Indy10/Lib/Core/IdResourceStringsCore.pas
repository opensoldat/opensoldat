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
  Rev 1.5    12/2/2004 9:26:44 PM  JPMugaas
  Bug fix.

  Rev 1.4    11/11/2004 10:25:24 PM  JPMugaas
  Added OpenProxy and CloseProxy so you can do RecvFrom and SendTo functions
  from the UDP client with SOCKS.  You must call OpenProxy  before using
  RecvFrom or SendTo.  When you are finished, you must use CloseProxy to close
  any connection to the Proxy.  Connect and disconnect also call OpenProxy and
  CloseProxy.

  Rev 1.3    11/11/2004 3:42:52 AM  JPMugaas
  Moved strings into RS.  Socks will now raise an exception if you attempt to
  use SOCKS4 and SOCKS4A with UDP.  Those protocol versions do not support UDP
  at all.

  Rev 1.2    2004.05.20 11:39:12 AM  czhower
  IdStreamVCL

  Rev 1.1    6/4/2004 5:13:26 PM  SGrobety
  EIdMaxCaptureLineExceeded message string

  Rev 1.0    2004.02.03 4:19:50 PM  czhower
  Rename

  Rev 1.15    10/24/2003 4:21:56 PM  DSiders
  Addes resource string for stream read exception.

  Rev 1.14    2003.10.16 11:25:22 AM  czhower
  Added missing ;

  Rev 1.13    10/15/2003 11:11:06 PM  DSiders
  Added resource srting for exception raised in TIdTCPServer.SetScheduler.

  Rev 1.12    10/15/2003 11:03:00 PM  DSiders
  Added resource string for circular links from transparent proxy.
  Corrected spelling errors.

  Rev 1.11    10/15/2003 10:41:34 PM  DSiders
  Added resource strings for TIdStream and TIdStreamProxy exceptions.

  Rev 1.10    10/15/2003 8:48:56 PM  DSiders
  Added resource strings for exceptions raised when setting thread component
  properties.

  Rev 1.9    10/15/2003 8:35:28 PM  DSiders
  Added resource string for exception raised in TIdSchedulerOfThread.NewYarn.

  Rev 1.8    10/15/2003 8:04:26 PM  DSiders
  Added resource strings for exceptions raised in TIdLogFile, TIdReply, and
  TIdIOHandler.

  Rev 1.7    10/15/2003 1:03:42 PM  DSiders
  Created resource strings for TIdBuffer.Find exceptions.

  Rev 1.6    2003.10.14 1:26:44 PM  czhower
  Uupdates + Intercept support

  Rev 1.5    10/1/2003 10:49:02 PM  GGrieve
  Rework buffer for Octane Compability

  Rev 1.4    7/1/2003 8:32:32 PM  BGooijen
  Added RSFibersNotSupported

  Rev 1.3    7/1/2003 02:31:34 PM  JPMugaas
  Message for invalid IP address.

  Rev 1.2    5/14/2003 6:40:22 PM  BGooijen
  RS for transparent proxy

  Rev 1.1    1/17/2003 05:06:04 PM  JPMugaas
  Exceptions for scheduler string.

  Rev 1.0    11/13/2002 08:42:02 AM  JPMugaas
}

unit IdResourceStringsCore;

interface

{$i IdCompilerDefines.inc}

resourcestring
  RSNoBindingsSpecified = 'No bindings specified.';
  RSCannotAllocateSocket = 'Cannot allocate socket.';
  RSSocksUDPNotSupported = 'UDP is not support in this SOCKS version.';
  RSSocksRequestFailed = 'Request rejected or failed.';
  RSSocksRequestServerFailed = 'Request rejected because SOCKS server cannot connect.';
  RSSocksRequestIdentFailed = 'Request rejected because the client program and identd report different user-ids.';
  RSSocksUnknownError = 'Unknown socks error.';
  RSSocksServerRespondError = 'Socks server did not respond.';
  RSSocksAuthMethodError = 'Invalid socks authentication method.';
  RSSocksAuthError = 'Authentication error to socks server.';
  RSSocksServerGeneralError = 'General SOCKS server failure.';
  RSSocksServerPermissionError = 'Connection not allowed by ruleset.';
  RSSocksServerNetUnreachableError = 'Network unreachable.';
  RSSocksServerHostUnreachableError = 'Host unreachable.';
  RSSocksServerConnectionRefusedError = 'Connection refused.';
  RSSocksServerTTLExpiredError = 'TTL expired.';
  RSSocksServerCommandError = 'Command not supported.';
  RSSocksServerAddressError = 'Address type not supported.';
  RSInvalidIPAddress = 'Invalid IP Address';
  RSInterceptCircularLink = '%s: Circular links are not allowed';

  RSNotEnoughDataInBuffer = 'Not enough data in buffer. (%d/%d)';
  RSTooMuchDataInBuffer = 'Too much data in buffer.';
  RSCapacityTooSmall = 'Capacity cannot be smaller than Size.';
  RSBufferIsEmpty = 'No bytes in buffer.';
  RSBufferRangeError = 'Index out of bounds.';

  RSFileNotFound = 'File "%s" not found';
  RSNotConnected = 'Not Connected';
  RSObjectTypeNotSupported = 'Object type not supported.';
  RSIdNoDataToRead = 'No data to read.';
  RSReadTimeout = 'Read timed out.';
  RSReadLnWaitMaxAttemptsExceeded = 'Max line read attempts exceeded.';
  RSAcceptTimeout = 'Accept timed out.';
  RSReadLnMaxLineLengthExceeded = 'Max line length exceeded.';
  RSRequiresLargeStream = 'Set LargeStream to True to send streams greater than 2GB';
  RSDataTooLarge = 'Data is too large for stream';
  RSConnectTimeout = 'Connect timed out.';
  RSICMPNotEnoughtBytes = 'Not enough bytes received';
  RSICMPNonEchoResponse = 'Non-echo type response received';
  RSThreadTerminateAndWaitFor  = 'Cannot call TerminateAndWaitFor on FreeAndTerminate threads';
  RSAlreadyConnected = 'Already connected.';
  RSTerminateThreadTimeout = 'Terminate Thread Timeout';
  RSNoExecuteSpecified = 'No execute handler found.';
  RSNoCommandHandlerFound = 'No command handler found.';
  RSCannotPerformTaskWhileServerIsActive = 'Cannot perform task while server is active.';
  RSThreadClassNotSpecified = 'Thread Class Not Specified.';
  RSMaximumNumberOfCaptureLineExceeded = 'Maximum number of line allowed exceeded'; // S.G. 6/4/2004: IdIOHandler.DoCapture
  RSNoCreateListeningThread = 'Cannot create listening thread.';
  RSInterceptIsDifferent = 'The IOHandler already has a different Intercept assigned';

  //scheduler
  RSchedMaxThreadEx = 'The maximum number of threads for this scheduler is exceeded.';
  //transparent proxy
  RSTransparentProxyCannotBind = 'Transparent proxy cannot bind.';
  RSTransparentProxyCanNotSupportUDP = 'UDP Not supported by this proxy.';
  //Fibers
  RSFibersNotSupported = 'Fibers are not supported on this system.';
  // TIdICMPCast
  RSIPMCastInvalidMulticastAddress = 'The supplied IP address is not a valid multicast address.';
  RSIPMCastNotSupportedOnWin32 = 'This function is not supported on Win32.';
  RSIPMCastReceiveError0 = 'IP Broadcast Receive Error = 0.';

  // Log strings
  RSLogConnected = 'Connected.';
  RSLogDisconnected = 'Disconnected.';
  RSLogEOL = '<EOL>';  // End of Line
  RSLogCR  = '<CR>';   // Carriage Return
  RSLogLF  = '<LF>';   // Line feed
  RSLogRecv = 'Recv '; // Receive
  RSLogSent = 'Sent '; // Send
  RSLogStat = 'Stat '; // Status

  RSLogFileAlreadyOpen = 'Unable to set Filename while log file is open.';

  RSBufferMissingTerminator = 'Buffer terminator must be specified.';
  RSBufferInvalidStartPos   = 'Buffer start position is invalid.';

  RSIOHandlerCannotChange = 'Cannot change a connected IOHandler.';
  RSIOHandlerTypeNotInstalled = 'No IOHandler of type %s is installed.';

  RSReplyInvalidCode = 'Reply Code is not valid: %s';
  RSReplyCodeAlreadyExists = 'Reply Code already exists: %s';

  RSThreadSchedulerThreadRequired = 'Thread must be specified for the scheduler.';
  RSNoOnExecute = 'You must have an OnExecute event.';
  RSThreadComponentLoopAlreadyRunning = 'Cannot set Loop property when the Thread is already running.';
  RSThreadComponentThreadNameAlreadyRunning = 'Cannot set ThreadName when the Thread is already running.';

  RSStreamProxyNoStack = 'A Stack has not been created for converting the data type.';

  RSTransparentProxyCyclic = 'Transparent Proxy Cyclic error.';

  RSTCPServerSchedulerAlreadyActive = 'Cannot change the scheduler while the server is Active.';
  RSUDPMustUseProxyOpen = 'You must use proxyOpen';

//ICMP stuff
  RSICMPTimeout = 'Timeout';
//Destination Address -3
  RSICMPNetUnreachable  = 'net unreachable;';
  RSICMPHostUnreachable = 'host unreachable;';
  RSICMPProtUnreachable = 'protocol unreachable;';
  RSICMPPortUnreachable = 'Port Unreachable';
  RSICMPFragmentNeeded = 'Fragmentation Needed and Don''t Fragment was Set';
  RSICMPSourceRouteFailed = 'Source Route Failed';
  RSICMPDestNetUnknown = 'Destination Network Unknown';
  RSICMPDestHostUnknown = 'Destination Host Unknown';
  RSICMPSourceIsolated = 'Source Host Isolated';
  RSICMPDestNetProhibitted = 'Communication with Destination Network is Administratively Prohibited';
  RSICMPDestHostProhibitted = 'Communication with Destination Host is Administratively Prohibited';
  RSICMPTOSNetUnreach =  'Destination Network Unreachable for Type of Service';
  RSICMPTOSHostUnreach = 'Destination Host Unreachable for Type of Service';
  RSICMPAdminProhibitted = 'Communication Administratively Prohibited';
  RSICMPHostPrecViolation = 'Host Precedence Violation';
  RSICMPPrecedenceCutoffInEffect =  'Precedence cutoff in effect';
  //for IPv6
  RSICMPNoRouteToDest = 'no route to destination';
  RSICMPAAdminDestProhibitted =  'communication with destination administratively prohibited';
  RSICMPSourceFilterFailed = 'source address failed ingress/egress policy';
  RSICMPRejectRoutToDest = 'reject route to destination';
  // Destination Address - 11
  RSICMPTTLExceeded     = 'time to live exceeded in transit';
  RSICMPHopLimitExceeded = 'hop limit exceeded in transit';
  RSICMPFragAsmExceeded = 'fragment reassembly time exceeded.';
//Parameter Problem - 12
  RSICMPParamError      = 'Parameter Problem (offset %d)';
  //IPv6
  RSICMPParamHeader = 'erroneous header field encountered (offset %d)';
  RSICMPParamNextHeader = 'unrecognized Next Header type encountered (offset %d)';
  RSICMPUnrecognizedOpt = 'unrecognized IPv6 option encountered (offset %d)';
//Source Quench Message -4
  RSICMPSourceQuenchMsg = 'Source Quench Message';
//Redirect Message
  RSICMPRedirNet =        'Redirect datagrams for the Network.';
  RSICMPRedirHost =       'Redirect datagrams for the Host.';
  RSICMPRedirTOSNet =     'Redirect datagrams for the Type of Service and Network.';
  RSICMPRedirTOSHost =    'Redirect datagrams for the Type of Service and Host.';
//echo
  RSICMPEcho = 'Echo';
//timestamp
  RSICMPTimeStamp = 'Timestamp';
//information request
  RSICMPInfoRequest = 'Information Request';
//mask request
  RSICMPMaskRequest = 'Address Mask Request';
// Traceroute
  RSICMPTracePacketForwarded = 'Outbound Packet successfully forwarded';
  RSICMPTraceNoRoute = 'No route for Outbound Packet; packet discarded';
//conversion errors
  RSICMPConvUnknownUnspecError = 'Unknown/unspecified error';
  RSICMPConvDontConvOptPresent = 'Don''t Convert option present';
  RSICMPConvUnknownMandOptPresent =  'Unknown mandatory option present';
  RSICMPConvKnownUnsupportedOptionPresent = 'Known unsupported option present';
  RSICMPConvUnsupportedTransportProtocol = 'Unsupported transport protocol';
  RSICMPConvOverallLengthExceeded = 'Overall length exceeded';
  RSICMPConvIPHeaderLengthExceeded = 'IP header length exceeded';
  RSICMPConvTransportProtocol_255 = 'Transport protocol > 255';
  RSICMPConvPortConversionOutOfRange = 'Port conversion out of range';
  RSICMPConvTransportHeaderLengthExceeded = 'Transport header length exceeded';
  RSICMPConv32BitRolloverMissingAndACKSet = '32 Bit Rollover missing and ACK set';
  RSICMPConvUnknownMandatoryTransportOptionPresent =      'Unknown mandatory transport option present';
//mobile host redirect
  RSICMPMobileHostRedirect = 'Mobile Host Redirect';
//IPv6 - Where are you
  RSICMPIPv6WhereAreYou    = 'IPv6 Where-Are-You';
//IPv6 - I am here
  RSICMPIPv6IAmHere        = 'IPv6 I-Am-Here';
// Mobile Regestration request
  RSICMPMobReg             = 'Mobile Registration Request';
//Skip
  RSICMPSKIP               = 'SKIP';
//Security
  RSICMPSecBadSPI          = 'Bad SPI';
  RSICMPSecAuthenticationFailed = 'Authentication Failed';
  RSICMPSecDecompressionFailed = 'Decompression Failed';
  RSICMPSecDecryptionFailed = 'Decryption Failed';
  RSICMPSecNeedAuthentication = 'Need Authentication';
  RSICMPSecNeedAuthorization = 'Need Authorization';
//IPv6 Packet Too Big
  RSICMPPacketTooBig = 'Packet Too Big (MTU = %d)';
{ TIdCustomIcmpClient }

  // TIdSimpleServer
  RSCannotUseNonSocketIOHandler = 'Cannot use a non-socket IOHandler';

implementation

end.
