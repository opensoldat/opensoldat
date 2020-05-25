
//since the array of const in delphi is not C compatible:
{$IFDEF FPC}

{**
 *  \brief The maximum size of a log message
 *
 *  Messages longer than the maximum size will be truncated
 *}
const
  SDL_MAX_LOG_MESSAGE = 4096;

{**
 *  \brief The predefined log categories
 *
 *  By default the application category is enabled at the INFO level,
 *  the assert category is enabled at the WARN level, test is enabled
 *  at the VERBOSE level and all other categories are enabled at the
 *  CRITICAL level.
 *}
type
  TSDL_LogCategory = (SDL_LOG_CATEGORY_APPLICATION,
					  SDL_LOG_CATEGORY_ERROR,
					  SDL_LOG_CATEGORY_ASSERT,
					  SDL_LOG_CATEGORY_SYSTEM,
					  SDL_LOG_CATEGORY_AUDIO,
				      SDL_LOG_CATEGORY_VIDEO,
					  SDL_LOG_CATEGORY_RENDER,
					  SDL_LOG_CATEGORY_INPUT,
					  SDL_LOG_CATEGORY_TEST,
					
					  {* Reserved for future SDL library use *}
					  SDL_LOG_CATEGORY_RESERVED1,
					  SDL_LOG_CATEGORY_RESERVED2,
					  SDL_LOG_CATEGORY_RESERVED3,
					  SDL_LOG_CATEGORY_RESERVED4,
					  SDL_LOG_CATEGORY_RESERVED5,
					  SDL_LOG_CATEGORY_RESERVED6,
					  SDL_LOG_CATEGORY_RESERVED7,
					  SDL_LOG_CATEGORY_RESERVED8,
					  SDL_LOG_CATEGORY_RESERVED9,
					  SDL_LOG_CATEGORY_RESERVED10,
					
					  {* Beyond this point is reserved for application use *}
					  SDL_LOG_CATEGORY_CUSTOM);

{**
 *  \brief The predefined log priorities
 *}
const
  SDL_LOG_PRIORITY_VERBOSE = 1;
  SDL_LOG_PRIORITY_DEBUG = 2;
  SDL_LOG_PRIORITY_INFO = 3;
  SDL_LOG_PRIORITY_WARN = 4;
  SDL_LOG_PRIORITY_ERROR = 5;
  SDL_LOG_PRIORITY_CRITICAL = 6;
  SDL_NUM_LOG_PRIORITIES = 7;
type 
  TSDL_LogPriority = Integer;


{**
 *  \brief Set the priority of all log categories
 *}
procedure SDL_LogSetAllPriority(priority: TSDL_LogPriority) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogSetAllPriority' {$ENDIF} {$ENDIF};

{**
 *  \brief Set the priority of a particular log category
 *}
procedure SDL_LogSetPriority(category: Integer; priority: TSDL_LogPriority) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogSetPriority' {$ENDIF} {$ENDIF};

{**
 *  \brief Get the priority of a particular log category
 *}
function SDL_LogGetPriority(category: Integer): TSDL_LogPriority cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogGetPriority' {$ENDIF} {$ENDIF};

{**
 *  \brief Reset all priorities to default.
 *
 *  \note This is called in SDL_Quit().
 *}
procedure SDL_LogResetPriorities() cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogResetPriorities' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_CATEGORY_APPLICATION and SDL_LOG_PRIORITY_INFO
 *}
procedure SDL_Log(const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_Log' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_PRIORITY_VERBOSE
 *}
procedure SDL_LogVerbose(category: Integer; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogVerbose' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_PRIORITY_DEBUG
 *}
procedure SDL_LogDebug(category: Integer; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogDebug' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_PRIORITY_INFO
 *}
procedure SDL_LogInfo(category: Integer; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogInfo' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_PRIORITY_WARN
 *}
procedure SDL_LogWarn(category: Integer; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogWarn' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_PRIORITY_ERROR
 *}
procedure SDL_LogError(category: Integer; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogError' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with SDL_LOG_PRIORITY_CRITICAL
 *}
procedure SDL_LogCritical(category: Integer; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogCritical' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with the specified category and priority.
 *}
procedure SDL_LogMessage(category: Integer; priority: TSDL_LogPriority; const fmt: PAnsiChar; pars: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogMessage' {$ENDIF} {$ENDIF};

{**
 *  \brief Log a message with the specified category and priority.
 *}
procedure SDL_LogMessageV(category: Integer; priority: TSDL_LogPriority; const fmt: PAnsiChar; ap: array of const) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogMessageV' {$ENDIF} {$ENDIF};

{**
 *  \brief The prototype for the log output function
 *}
type
  TSDL_LogOutputFunction = procedure(userdata: Pointer; category: Integer; priority: TSDL_LogPriority; const msg: PAnsiChar);
  PSDL_LogOutputFunction = ^TSDL_LogOutputFunction;
  
{**
 *  \brief Get the current log output function.
 *}
procedure SDL_LogGetOutputFunction(callback: PSDL_LogOutputFunction; userdata: PPointer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogGetOutputFunction' {$ENDIF} {$ENDIF};

{**
 *  \brief This function allows you to replace the default log output
 *         function with one of your own.
 *}
procedure SDL_LogSetOutputFunction(callback: TSDL_LogOutputFunction; userdata: Pointer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LogSetOutputFunction' {$ENDIF} {$ENDIF};

{$ENDIF}
