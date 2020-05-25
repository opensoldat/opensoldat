unit indycorefpc;
interface

uses
  IdAssignedNumbers,
  IdBuffer,
  IdCmdTCPClient,
  IdCmdTCPServer,
  IdCommandHandlers,
  IdContext,
  IdCustomTCPServer,
  IdCustomTransparentProxy,
  IdExceptionCore,
  IdGlobalCore,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdIOHandlerStream,
  IdIPAddress,
  IdIPMCastBase,
  IdIPMCastClient,
  IdIPMCastServer,
  IdIcmpClient,
  IdIntercept,
  IdInterceptSimLog,
  IdInterceptThrottler,
  IdLogBase,
  IdLogDebug,
  IdLogEvent,
  IdLogFile,
  IdLogStream,
  IdRawBase,
  IdRawClient,
  IdRawFunctions,
  IdRawHeaders,
  IdReply,
  IdReplyRFC,
  IdResourceStringsCore,
  IdScheduler,
  IdSchedulerOfThread,
  IdSchedulerOfThreadDefault,
  IdSchedulerOfThreadPool,
  IdServerIOHandler,
  IdServerIOHandlerSocket,
  IdServerIOHandlerStack,
  IdSimpleServer,
  IdSocketHandle,
  IdSocks,
  IdSync,
  IdTCPClient,
  IdTCPConnection,
  IdTCPServer,
  IdTCPStream,
  IdTask,
  IdThread,
  IdThreadComponent,
  IdThreadSafe,
  IdTraceRoute,
  IdUDPBase,
  IdUDPClient,
  IdUDPServer,
  IdYarn;

implementation

{
disable hints about unused units.  This unit just causes
FreePascal to compile implicitly listed units in a package.
}
{$hints off}
end.
