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
   Rev 1.2    2004.05.20 11:38:10 AM  czhower
 IdStreamVCL
}
{
   Rev 1.1    2004.02.03 3:15:54 PM  czhower
 Updates to move to System.
}
{
   Rev 1.0    2004.02.03 2:36:04 PM  czhower
 Move
}
unit IdResourceStrings;

interface

resourcestring
  //IIdTextEncoding
  RSInvalidSourceArray = 'Invalid source array';
  RSInvalidDestinationArray = 'Invalid destination array';
  RSCharIndexOutOfBounds = 'Character index out of bounds (%d)';
  RSInvalidCharCount = 'Invalid count (%d)';
  RSInvalidDestinationIndex = 'Invalid destination index (%d)';

  RSInvalidCodePage = 'Invalid codepage (%d)';
  RSInvalidCharSet = 'Invalid character set (%s)';
  RSInvalidCharSetConv = 'Invalid character set conversion (%s <-> %s)';
  RSInvalidCharSetConvWithFlags = 'Invalid character set conversion (%s <-> %s, %s)';

  RSUnsupportedCodePage = 'Unsupported codepage (%d)';
  RSUnsupportedCharSet = 'Unsupported character set (%s)';

  //IdSys
  RSFailedTimeZoneInfo = 'Failed attempting to retrieve time zone information.';
  // Winsock
  RSWinsockCallError = 'Error on call to Winsock2 library function %s';
  RSWinsockLoadError = 'Error on loading Winsock2 library (%s)';
  {CH RSWinsockInitializationError = 'Winsock Initialization Error.'; }
  // Status
  RSStatusResolving = 'Resolving hostname %s.';
  RSStatusConnecting = 'Connecting to %s.';
  RSStatusConnected = 'Connected.';
  RSStatusDisconnecting = 'Disconnecting.';
  RSStatusDisconnected = 'Disconnected.';
  RSStatusText = '%s';
  // Stack
  RSStackError = 'Socket Error # %d' + #13#10 + '%s';
  RSStackEINTR = 'Interrupted system call.';
  RSStackEBADF = 'Bad file number.';
  RSStackEACCES = 'Access denied.';
  RSStackEFAULT = 'Buffer fault.';
  RSStackEINVAL = 'Invalid argument.';
  RSStackEMFILE = 'Too many open files.';
  RSStackEWOULDBLOCK = 'Operation would block.';
  RSStackEINPROGRESS = 'Operation now in progress.';
  RSStackEALREADY = 'Operation already in progress.';
  RSStackENOTSOCK = 'Socket operation on non-socket.';
  RSStackEDESTADDRREQ = 'Destination address required.';
  RSStackEMSGSIZE = 'Message too long.';
  RSStackEPROTOTYPE = 'Protocol wrong type for socket.';
  RSStackENOPROTOOPT = 'Bad protocol option.';
  RSStackEPROTONOSUPPORT = 'Protocol not supported.';
  RSStackESOCKTNOSUPPORT = 'Socket type not supported.';
  RSStackEOPNOTSUPP = 'Operation not supported on socket.';
  RSStackEPFNOSUPPORT = 'Protocol family not supported.';
  RSStackEAFNOSUPPORT = 'Address family not supported by protocol family.';
  RSStackEADDRINUSE = 'Address already in use.';
  RSStackEADDRNOTAVAIL = 'Cannot assign requested address.';
  RSStackENETDOWN = 'Network is down.';
  RSStackENETUNREACH = 'Network is unreachable.';
  RSStackENETRESET = 'Net dropped connection or reset.';
  RSStackECONNABORTED = 'Software caused connection abort.';
  RSStackECONNRESET = 'Connection reset by peer.';
  RSStackENOBUFS = 'No buffer space available.';
  RSStackEISCONN = 'Socket is already connected.';
  RSStackENOTCONN = 'Socket is not connected.';
  RSStackESHUTDOWN = 'Cannot send or receive after socket is closed.';
  RSStackETOOMANYREFS = 'Too many references, cannot splice.';
  RSStackETIMEDOUT = 'Connection timed out.';
  RSStackECONNREFUSED = 'Connection refused.';
  RSStackELOOP = 'Too many levels of symbolic links.';
  RSStackENAMETOOLONG = 'File name too long.';
  RSStackEHOSTDOWN = 'Host is down.';
  RSStackEHOSTUNREACH = 'No route to host.';
  RSStackENOTEMPTY = 'Directory not empty';
  RSStackHOST_NOT_FOUND = 'Host not found.';
  RSStackClassUndefined = 'Stack Class is undefined.';
  RSStackAlreadyCreated = 'Stack already created.';
  // Other
  RSAntiFreezeOnlyOne = 'Only one TIdAntiFreeze can exist per application.';
  RSCannotSetIPVersionWhenConnected = 'Cannot change IPVersion when connected';
  RSCannotBindRange = 'Can not bind in port range (%d - %d)';
  RSConnectionClosedGracefully = 'Connection Closed Gracefully.';
  RSCorruptServicesFile = '%s is corrupt.';
  RSCouldNotBindSocket = 'Could not bind socket. Address and port are already in use.';
  RSInvalidPortRange = 'Invalid Port Range (%d - %d)';
  RSInvalidServiceName = '%s is not a valid service.';
  RSIPv6Unavailable = 'IPv6 unavailable';
  RSInvalidIPv6Address = '%s is not a valid IPv6 address';
  RSIPVersionUnsupported = 'The requested IPVersion / Address family is not supported.';
  RSNotAllBytesSent = 'Not all bytes sent.';
  RSPackageSizeTooBig = 'Package Size Too Big.';
  RSSetSizeExceeded = 'Set Size Exceeded.';
  RSNoEncodingSpecified = 'No encoding specified.';
  {CH RSStreamNotEnoughBytes = 'Not enough bytes read from stream.'; }
  RSEndOfStream = 'End of stream: Class %s at %d';

  //DNS Resolution error messages
  RSMaliciousPtrRecord = 'Malicious PTR Record';

implementation

end.
