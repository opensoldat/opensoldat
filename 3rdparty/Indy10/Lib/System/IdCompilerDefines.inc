{$IFDEF CONDITIONALEXPRESSIONS}
  // Must be at the top...
  {$IF CompilerVersion >= 24.0}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

// General

// Make this $DEFINE to use the 16 color icons required by Borland
// or DEFINE to use the 256 color Indy versions
{.$DEFINE Borland}

// S.G. 4/9/2002: IPv4/IPv6 general switch (for defaults only)
{$DEFINE IdIPv4} // use IPv4 by default
{.$IFDEF IdIPv6} // use IPv6 by default

{$DEFINE INDY100}
{$DEFINE 10_6_2}  //so developers can IFDEF for this specific version

// When generating C++Builder output files, certain workarounds to compiler
// problems need to be enabled! When invoking DCC on the command-line, use
// the -DBCB parameter. When invoking MSBUILD, include the DCC_Define="BCB"
// attribute in the /p parameter.
{$IFDEF BCB}
  {$DEFINE CBUILDER}
{$ELSE}
  {$DEFINE DELPHI}
{$ENDIF}

{$UNDEF USE_OPENSSL}
{$UNDEF STATICLOAD_OPENSSL}

{$UNDEF USE_ZLIB_UNIT}
{$UNDEF USE_SSPI}

// $DEFINE the following if the global objects in the IdStack and IdThread
// units should be freed on finalization
{.$DEFINE FREE_ON_FINAL}
{$UNDEF FREE_ON_FINAL}

// Make sure the following is $DEFINE'd only for suitable environments
// as specified further below.  This works in conjunction with the
// FREE_ON_FINAL define above.
{$UNDEF REGISTER_EXPECTED_MEMORY_LEAK}

// FastMM is natively available in BDS 2006 and higher.  $DEFINE the
// following if FastMM has been installed manually in earlier versions
{.$DEFINE USE_FASTMM4}
{$UNDEF USE_FASTMM4}

// $DEFINE the following if MadExcept has been installed manually in
// BDS 2005 or earlier (System.RegisterExpectedMemoryLeak() was introduced
// in BDS 2006)
{.$DEFINE USE_MADEXCEPT}
{$UNDEF USE_MADEXCEPT}

// Make sure the following are $DEFINE'd only for Delphi/C++Builder 2009 onwards
// as specified further below.  The VCL is fully Unicode, where the 'String'
// type maps to System.UnicodeString, not System.AnsiString anymore
{$UNDEF STRING_IS_UNICODE}
{$UNDEF STRING_IS_ANSI}
{$UNDEF STRING_UNICODE_MISMATCH}

// Make sure the following are $DEFINE'd only for suitable environments
// as specified further below.  Delphi/C++Builder Mobile/NextGen compilers
// do not support Ansi data types anymore, and is moving away from raw
// pointers as well.
{$DEFINE HAS_AnsiString}
{$DEFINE HAS_AnsiChar}
{$DEFINE HAS_PAnsiChar}
{$UNDEF HAS_PPAnsiChar}
{$UNDEF NO_ANSI_TYPES}
{$UNDEF USE_MARSHALLED_PTRS}
{$UNDEF HAS_MarshaledAString}
{$UNDEF USE_OBJECT_ARC}

// Make sure the following is $DEFINE'd only for suitable environments
// as specified further below.
{$UNDEF STRING_IS_IMMUTABLE}
{$UNDEF HAS_DIRECTIVE_ZEROBASEDSTRINGS}

// Make sure the following are $DEFINE'd only for suitable environments
// as specified further below.
{$UNDEF HAS_TEncoding}
{$UNDEF HAS_TEncoding_GetEncoding_ByEncodingName}
{$UNDEF HAS_Exception_RaiseOuterException}
{$UNDEF HAS_System_ReturnAddress}
{$UNDEF HAS_TCharacter}
{$UNDEF HAS_TInterlocked}
{$UNDEF HAS_TNetEncoding}

// Make sure that this is defined only for environments where we are using
// the iconv library to charactor conversions.
{.$UNDEF USE_ICONV}

//Define for Delphi cross-compiler targetting Posix
{$UNDEF USE_VCL_POSIX}
{$UNDEF HAS_ComponentPlatformsAttribute}
{$UNDEF HAS_ComponentPlatformsAttribute_Win32}
{$UNDEF HAS_ComponentPlatformsAttribute_Win64}
{$UNDEF HAS_ComponentPlatformsAttribute_OSX32}
{$UNDEF HAS_ComponentPlatformsAttribute_iOS_Simulator}
{$UNDEF HAS_ComponentPlatformsAttribute_Android}
{$UNDEF HAS_ComponentPlatformsAttribute_Linux32}
{$UNDEF HAS_ComponentPlatformsAttribute_iOS_Device}
{$UNDEF HAS_ComponentPlatformsAttribute_iOS_Device32}
{$UNDEF HAS_ComponentPlatformsAttribute_Linux64}
{$UNDEF HAS_ComponentPlatformsAttribute_WinNX32}
{$UNDEF HAS_ComponentPlatformsAttribute_WinIoT32}
{$UNDEF HAS_ComponentPlatformsAttribute_iOS_Device64}
{$UNDEF HAS_DIRECTIVE_WARN_DEFAULT}

// Define for Delphi to auto-generate platform-appropriate '#pragma link' statements in HPP files
{$UNDEF HAS_DIRECTIVE_HPPEMIT_LINKUNIT}

// detect compiler versions

{$IFNDEF FPC}

  // TODO: to detect features in Delphi/C++Builder v6 and later, use CompilerVersion
  // and RTLVersion constants instead of VERXXX defines.  We still support v5, which
  // does not have such constants.

  // Delphi 4
  {$IFDEF VER120}
    {$DEFINE DCC}
    {$DEFINE VCL_40}
    {$DEFINE DELPHI_4}
  {$ENDIF}

  // C++Builder 4
  {$IFDEF VER125}
    {$DEFINE DCC}
    {$DEFINE VCL_40}
    {$DEFINE CBUILDER_4}
  {$ENDIF}

  // Delphi & C++Builder 5
  {$IFDEF VER130}
    {$DEFINE DCC}
    {$DEFINE VCL_50}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_5}
    {$ELSE}
      {$DEFINE DELPHI_5}
    {$ENDIF}
  {$ENDIF}

  //Delphi & C++Builder 6
  {$IFDEF VER140}
    {$DEFINE DCC}
    {$DEFINE VCL_60}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_6}
    {$ELSE}
      {$DEFINE DELPHI_6}
    {$ENDIF}
  {$ENDIF}

  //Delphi 7
  {$IFDEF VER150}
    {$DEFINE DCC}
    {$DEFINE VCL_70}
    {$DEFINE DELPHI_7} // there was no C++ Builder 7
  {$ENDIF}

  //Delphi 8
  {$IFDEF VER160}
    {$DEFINE DCC}
    {$DEFINE VCL_80}
    {$DEFINE DELPHI_8} // there was no C++ Builder 8
  {$ENDIF}

  //Delphi 2005
  {$IFDEF VER170}
    {$DEFINE DCC}
    {$DEFINE VCL_2005}
    {$DEFINE DELPHI_2005} // there was no C++Builder 2005
  {$ENDIF}

  // NOTE: CodeGear decided to make Highlander be a non-breaking release
  // (no interface changes, thus fully backwards compatible without any
  // end user code changes), so VER180 applies to both BDS 2006 and
  // Highlander prior to the release of RAD Studio 2007.  Use VER185 to
  // identify Highlanger specifically.

  //Delphi & C++Builder 2006
  //Delphi & C++Builder 2007 (Highlander)
  {$IFDEF VER180}
    {$DEFINE DCC}
    {$DEFINE VCL_2006}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_2006}
    {$ELSE}
      {$DEFINE DELPHI_2006}
    {$ENDIF}
  {$ENDIF}

  //Delphi & C++Builder 2007 (Highlander)
  {$IFDEF VER185}
    {$DEFINE DCC}
    {$UNDEF VCL_2006}
    {$DEFINE VCL_2007}
    {$IFDEF CBUILDER}
      {$UNDEF CBUILDER_2006}
      {$DEFINE CBUILDER_2007}
    {$ELSE}
      {$UNDEF DELPHI_2006}
      {$DEFINE DELPHI_2007}
    {$ENDIF}
  {$ENDIF}

  // BDS 2007 NET personality uses VER190 instead of 185.
  //Delphi .NET 2007
  {$IFDEF VER190}
    {$DEFINE DCC}
    {$IFDEF CIL}
      //Delphi 2007
      {$DEFINE VCL_2007}
      {$DEFINE DELPHI_2007}
    {$ENDIF}
  {$ENDIF}

  //Delphi & C++Builder 2009 (Tiburon)
  {$IFDEF VER200}
    {$DEFINE DCC}
    {$DEFINE VCL_2009}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_2009}
    {$ELSE}
      {$DEFINE DELPHI_2009}
    {$ENDIF}
  {$ENDIF}

  //Delphi & C++Builder 2010 (Weaver)
  {$IFDEF VER210}
    {$DEFINE DCC}
    {$DEFINE VCL_2010}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_2010}
    {$ELSE}
      {$DEFINE DELPHI_2010}
    {$ENDIF}
  {$ENDIF}

  //Delphi & C++Builder XE (Fulcrum)
  {$IFDEF VER220}
  //REMOVE DCC DEFINE after the next Fulcrum beta.
  //It will be defined there.
    {$IFNDEF DCC}
      {$DEFINE DCC}
    {$ENDIF}
    {$DEFINE VCL_XE}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE}
    {$ELSE}
      {$DEFINE DELPHI_XE}
    {$ENDIF}
  {$ENDIF}

  // DCC is now defined by the Delphi compiler starting in XE2

  //Delphi & CBuilder XE2 (Pulsar)
  {$IFDEF VER230}
    {$DEFINE VCL_XE2}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE2}
    {$ELSE}
      {$DEFINE DELPHI_XE2}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder XE3 (Waterdragon)
  //Delphi & CBuilder XE3.5 (Quintessence - early betas only)
  {$IFDEF VER240}
    {$DEFINE VCL_XE3}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE3}
    {$ELSE}
      {$DEFINE DELPHI_XE3}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder XE4 (Quintessence)
  {$IFDEF VER250}
    {$UNDEF VCL_XE3}
    {$DEFINE VCL_XE4}
    {$IFDEF CBUILDER}
      {$UNDEF CBUILDER_XE3}
      {$DEFINE CBUILDER_XE4}
    {$ELSE}
      {$UNDEF DELPHI_XE3}
      {$DEFINE DELPHI_XE4}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder XE5 (Zephyr)
  {$IFDEF VER260}
    {$DEFINE VCL_XE5}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE5}
    {$ELSE}
      {$DEFINE DELPHI_XE5}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder AppMethod
  //AppMethod is just XE5 for mobile only, VCL is removed
  {$IFDEF VER265}
    {$DEFINE VCL_XE5}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE5}
    {$ELSE}
      {$DEFINE DELPHI_XE5}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder XE6 (Proteus)
  {$IFDEF VER270}
    {$DEFINE VCL_XE6}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE6}
    {$ELSE}
      {$DEFINE DELPHI_XE6}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder XE7 (Carpathia)
  {$IFDEF VER280}
    {$DEFINE VCL_XE7}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE7}
    {$ELSE}
      {$DEFINE DELPHI_XE7}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder XE8 (Elbrus)
  {$IFDEF VER290}
    {$DEFINE VCL_XE8}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_XE8}
    {$ELSE}
      {$DEFINE DELPHI_XE8}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder 10.0 Seattle (Aitana)
  {$IFDEF VER300}
    {$DEFINE VCL_10_0_SEATTLE}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_VCL_10_0_SEATTLE}
    {$ELSE}
      {$DEFINE DELPHI_VCL_10_0_SEATTLE}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder 10.1 Berlin (BigBen)
  {$IFDEF VER310}
    {$DEFINE VCL_10_1_BERLIN}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_10_1_BERLIN}
    {$ELSE}
      {$DEFINE DELPHI_10_1_BERLIN}
    {$ENDIF}
  {$ENDIF}

  //Delphi & CBuilder 10.2 Tokyo (Godzilla)
  {$IFDEF VER320}
    {$DEFINE VCL_10_2_TOKYO}
    {$IFDEF CBUILDER}
      {$DEFINE CBUILDER_10_2_TOKYO}
    {$ELSE}
      {$DEFINE DELPHI_10_2_TOKYO}
    {$ENDIF}
  {$ENDIF}

  // Kylix
  //
  //Important:  Don't use CompilerVersion here as IF's are evaluated before
  //IFDEF's and Kylix 1 does not have CompilerVersion defined at all.
  {$IFDEF LINUX}
    {$DEFINE UNIX}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IF (RTLVersion >= 14.0) and (RTLVersion <= 14.5) }
        {$DEFINE KYLIX}
        {$IF RTLVersion = 14.5}
          {$DEFINE KYLIX_3}
        {$ELSEIF RTLVersion >= 14.2}
          {$DEFINE KYLIX_2}
        {$ELSE}
          {$DEFINE KYLIX_1}
        {$IFEND}
      {$IFEND}
    {$ENDIF}
  {$ENDIF}

{$ENDIF}

// Delphi.NET
// Covers D8+
{$IFDEF CIL}
  // Platform specific conditional. Used for platform specific code.
  {$DEFINE DOTNET}
  {$DEFINE STRING_IS_UNICODE}
  {$DEFINE STRING_IS_IMMUTABLE}
  {.$DEFINE HAS_Int8}
  {.$DEFINE HAS_UInt8}
  {$DEFINE HAS_Int16}
  {$DEFINE HAS_UInt16}
  {$DEFINE HAS_Int32}
  {$DEFINE HAS_UInt32}
  {$DEFINE HAS_UInt64}
{$ENDIF}

{$IFDEF KYLIX}
  {$DEFINE VCL_60}
  {$DEFINE INT_THREAD_PRIORITY}
  {$DEFINE CPUI386}
  {$UNDEF USE_BASEUNIX}

  {$IFDEF KYLIX_3}
    {$DEFINE KYLIX_3_OR_ABOVE}
  {$ENDIF}

  {$IFDEF KYLIX_3_OR_ABOVE}
    {$DEFINE KYLIX_2_OR_ABOVE}
  {$ELSE}
    {$IFDEF KYLIX_2}
      {$DEFINE KYLIX_2_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF KYLIX_2_OR_ABOVE}
    {$DEFINE KYLIX_1_OR_ABOVE}
  {$ELSE}
    {$IFDEF KYLIX_1}
      {$DEFINE KYLIX_1_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF KYLIX_3_OR_ABOVE}
    {$DEFINE KYLIXCOMPAT}
  {$ENDIF}

  {$IFDEF KYLIX_2_OR_ABOVE}
    {$DEFINE USE_ZLIB_UNIT}
  {$ENDIF}
{$ENDIF}

// FPC (2+)

{$IFDEF FPC}
  // TODO: In FreePascal 4.2.0+, a Delphi-like UnicodeString type is supported.
  // However, String/(P)Char do not map to UnicodeString/(P)WideChar unless
  // either {$MODE DelphiUnicode} or {$MODESWITCH UnicodeStrings} is used.
  // We should consider enabling one of them so Indy uses the same Unicode logic
  // in Delphi 2009+ and FreePascal 4.2.0+ and reduces IFDEFs (in particular,
  // STRING_UNICODE_MISMATCH, see further below).  However, FreePascal's RTL
  // is largely not UnicodeString-enabled yet.  Maybe we should enable
  // {$MODE DelphiUnicode} anyway, and then deal with any RTL function issues
  // on an as-needed basis...
  {$MODE Delphi}
  {$IFDEF FPC_2_7_1_OR_ABOVE}
    {.$MODE DelphiUnicode}
    {.$MODESWITCH UnicodeStrings}
  {$ENDIF}
  //note that we may need further defines for widget types depending on
  //what we do and what platforms we support in FPC.
  //I'll let Marco think about that one.
  {$IFDEF UNIX}
    {$DEFINE USE_BASEUNIX}
    {$IFDEF LINUX}
      //In Linux for I386, you can choose between a Kylix-libc API or
      //the standard RTL Unix API.  Just pass -dKYLIXCOMPAT to the FPC compiler.
      //I will see what I can do about the Makefile.
      {$IFDEF KYLIXCOMPAT}
        {$IFDEF CPUI386}
          {$UNDEF USE_BASEUNIX}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF USE_BASEUNIX}
      {$UNDEF KYLIXCOMPAT}
    {$ENDIF}
  {$ENDIF}

  // FPC_FULLVERSION was added in FPC 2.2.4
  // Have to use Defined() or else Delphi compiler chokes, since it
  // evaluates $IF statements before $IFDEF statements...

  {$MACRO ON} // must be on in order to use versioning macros
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30101)}
    {$DEFINE FPC_3_1_1_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30000)}
    {$DEFINE FPC_3_0_0_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20701)}
    {$DEFINE FPC_2_7_1_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20604)}
    {$DEFINE FPC_2_6_4_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20602)}
    {$DEFINE FPC_2_6_2_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20600)}
    {$DEFINE FPC_2_6_0_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20404)}
    {$DEFINE FPC_2_4_4_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20402)}
    {$DEFINE FPC_2_4_2_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20400)}
    {$DEFINE FPC_2_4_0_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20204)}
    {$DEFINE FPC_2_2_4_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20202)}
    {$DEFINE FPC_2_2_2_OR_ABOVE}
  {$IFEND}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20105)}
    {$DEFINE FPC_2_1_5_OR_ABOVE}
  {$IFEND}

  // just in case
  {$IFDEF FPC_3_1_1}
    {$DEFINE FPC_3_1_1_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_3_0_0}
    {$DEFINE FPC_3_0_0_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_7_1}
    {$DEFINE FPC_2_7_1_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_6_4}
    {$DEFINE FPC_2_6_4_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_6_2}
    {$DEFINE FPC_2_6_2_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_6_0}
    {$DEFINE FPC_2_6_0_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_4_4}
    {$DEFINE FPC_2_4_4_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_4_2}
    {$DEFINE FPC_2_4_2_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_4_0}
    {$DEFINE FPC_2_4_0_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_2_4}
    {$DEFINE FPC_2_2_4_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_2_2}
    {$DEFINE FPC_2_2_2_OR_ABOVE}
  {$ENDIF}
  {$IFDEF FPC_2_1_5}
    {$DEFINE FPC_2_1_5_OR_ABOVE}
  {$ENDIF}

  {$IFDEF FPC_3_1_1_OR_ABOVE}
    {$DEFINE FPC_3_0_0_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_3_0_0}
      {$DEFINE FPC_3_0_0_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_3_0_0_OR_ABOVE}
    {$DEFINE FPC_2_7_1_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_7_1}
      {$DEFINE FPC_2_7_1_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_7_1_OR_ABOVE}
    {$DEFINE FPC_2_6_4_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_6_4}
      {$DEFINE FPC_2_6_4_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_6_4_OR_ABOVE}
    {$DEFINE FPC_2_6_2_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_6_2}
      {$DEFINE FPC_2_6_2_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_6_2_OR_ABOVE}
    {$DEFINE FPC_2_6_0_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_6_0}
      {$DEFINE FPC_2_6_0_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_6_0_OR_ABOVE}
    {$DEFINE FPC_2_4_4_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_4_4}
      {$DEFINE FPC_2_4_4_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_4_4_OR_ABOVE}
    {$DEFINE FPC_2_4_2_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_4_2}
      {$DEFINE FPC_2_4_2_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_4_2_OR_ABOVE}
    {$DEFINE FPC_2_4_0_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_4_0}
      {$DEFINE FPC_2_4_0_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_4_0_OR_ABOVE}
    {$DEFINE FPC_2_2_4_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_2_4}
      {$DEFINE FPC_2_2_4_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_2_4_OR_ABOVE}
    {$DEFINE FPC_2_2_2_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_2_2}
      {$DEFINE FPC_2_2_2_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_2_2_OR_ABOVE}
    {$DEFINE FPC_2_2_0_OR_ABOVE}
  {$ELSE}
    {$IFDEF VER2_2}
      {$DEFINE FPC_2_2_0_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC_2_2_0_OR_ABOVE}
    {$DEFINE FPC_2_1_5_OR_ABOVE}
  {$ELSE}
    {$IFDEF FPC_2_1_5}
      {$DEFINE FPC_2_1_5_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {.$IFDEF FPC_2_7_1_OR_ABOVE}
    // support for RawByteString and UnicodeString
    {.$DEFINE VCL_2009}
    {.$DEFINE DELPHI_2009}
  {.$ELSE}
    {$DEFINE VCL_70}
    {$DEFINE DELPHI_7}
  {.$ENDIF}
{$ENDIF}

// end FPC

{$IFDEF VCL_10_2_TOKYO}
  {$DEFINE VCL_10_2_TOKYO_OR_ABOVE}
{$ENDIF}

{$IFDEF VCL_10_2_TOKYO_OR_ABOVE}
  {$DEFINE VCL_10_1_BERLIN_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_10_1_BERLIN}
    {$DEFINE VCL_10_1_BERLIN_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_10_1_BERLIN_OR_ABOVE}
  {$DEFINE VCL_10_0_SEATTLE_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_10_0_SEATTLE}
    {$DEFINE VCL_10_0_SEATTLE_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_10_0_SEATTLE_OR_ABOVE}
  {$DEFINE VCL_XE8_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE8}
    {$DEFINE VCL_XE8_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE8_OR_ABOVE}
  {$DEFINE VCL_XE7_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE7}
    {$DEFINE VCL_XE7_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE7_OR_ABOVE}
  {$DEFINE VCL_XE6_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE6}
    {$DEFINE VCL_XE6_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE6_OR_ABOVE}
  {$DEFINE VCL_XE5_OR_ABOVE}
  {$DEFINE VCL_XE5_UPDATE2_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE5}
    {$DEFINE VCL_XE5_OR_ABOVE}
    // TODO: figure out how to detect this version
    {.$DEFINE VCL_XE5_UPDATE2_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE5_OR_ABOVE}
  {$DEFINE VCL_XE4_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE4}
    {$DEFINE VCL_XE4_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE4_OR_ABOVE}
  {$DEFINE VCL_XE3_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE3}
    {$DEFINE VCL_XE3_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE3_OR_ABOVE}
  {$DEFINE VCL_XE2_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE2}
    {$DEFINE VCL_XE2_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE2_OR_ABOVE}
  {$DEFINE VCL_XE_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE}
    {$DEFINE VCL_XE_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE_OR_ABOVE}
  {$DEFINE VCL_2010_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2010}
    {$DEFINE VCL_2010_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2010_OR_ABOVE}
  {$DEFINE VCL_2009_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2009}
    {$DEFINE VCL_2009_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2009_OR_ABOVE}
  {$DEFINE VCL_2007_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2007}
    {$DEFINE VCL_2007_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2007_OR_ABOVE}
  {$DEFINE VCL_2006_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2006}
    {$DEFINE VCL_2006_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2006_OR_ABOVE}
  {$DEFINE VCL_2005_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2005}
    {$DEFINE VCL_2005_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2005_OR_ABOVE}
  {$DEFINE VCL_8_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_80}
    {$DEFINE VCL_8_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_8_OR_ABOVE}
  {$DEFINE VCL_7_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_70}
    {$DEFINE VCL_7_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_7_OR_ABOVE}
  {$DEFINE VCL_6_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_60}
    {$DEFINE VCL_6_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_6_OR_ABOVE}
  {$DEFINE VCL_5_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_50}
    {$DEFINE VCL_5_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_5_OR_ABOVE}
  {$DEFINE VCL_4_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_40}
    {$DEFINE VCL_4_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

// Normalize Delphi compiler defines to match FPC for consistency:
//
// CPU32 - any 32-bit CPU
// CPU64 - any 64-bit CPU
// WINDOWS - any Windows platform (32-bit, 64-bit, CE)
// WIN32 - Windows 32-bit
// WIN64 - Windows 64-bit
// WINCE - Windows CE
//
// Consult the "Free Pascal Programmer's Guide", Appendix G for the complete
// list of defines that are used. Do not work on this unless you understand
// what the FreePascal developers are doing.  Not only do you have to
// descriminate with operating systems, but also with chip architectures
// are well.
//
// DCC Pulsar+ define the following values:
// ASSEMBLER
// DCC
// CONDITIONALEXPRESSIONS
// NATIVECODE
// UNICODE
// MACOS
// MACOS32
// MACOS64
// MSWINDOWS
// WIN32
// WIN64
// LINUX
// POSIX
// POSIX32
// CPU386
// CPUX86
// CPUX64
//
// Kylix defines the following values:
// LINUX
// (others??)
//

{$IFNDEF FPC}
  // TODO: We need to use ENDIAN_BIG for big endian chip architectures,
  // such as 680x0, PowerPC, Sparc, and MIPS, once DCC supports them,
  // provided it does not already define its own ENDIAN values by then...
  {$DEFINE ENDIAN_LITTLE}
  {$IFNDEF VCL_6_OR_ABOVE}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF}
  // TODO: map Pulsar's non-Windows platform defines...
  {$IFDEF VCL_XE2_OR_ABOVE}
    {$IFDEF CPU386}
      //any 32-bit CPU
      {$DEFINE CPU32}
      //Intel 386 compatible chip architecture
      {$DEFINE CPUI386}
    {$ENDIF}
    {$IFDEF CPUX86}
      {$DEFINE CPU32}
    {$ENDIF}
    {$IFDEF CPUX64}
      //any 64-bit CPU
      {$DEFINE CPU64}
      //AMD64 compatible chip architecture
      {$DEFINE CPUX86_64}  //historical name for AMD64
      {$DEFINE CPUAMD64}
    {$ENDIF}
  {$ELSE}
    {$IFNDEF DOTNET}
      {$IFNDEF KYLIX}
        {$DEFINE I386}
      {$ENDIF}
    {$ENDIF}
    {$DEFINE CPU32}
  {$ENDIF}
{$ENDIF}

{$IFDEF DOTNET}
  //differences in DotNET Framework versions.
  {$IFDEF VCL_2007_OR_ABOVE}
    {$DEFINE DOTNET_2}
    {$DEFINE DOTNET_2_OR_ABOVE}
  {$ELSE}
    {$DEFINE DOTNET_1_1}
  {$ENDIF}
  {$DEFINE DOTNET_1_1_OR_ABOVE}
  // Extra include used in D7 for testing. Remove later when all comps are
  // ported. Used to selectively exclude non ported parts. Allowed in places
  // IFDEFs are otherwise not permitted.
  {$DEFINE DOTNET_EXCLUDE}
{$ENDIF}

// Check for available features

{$IFDEF CBUILDER}
  // When generating a C++ HPP file, if a class has no explicit constructor
  // defined and contains compiler-managed members (xxxString, TDateTime,
  // Variant, DelphiInterface, etc), the HPP will contain a forwarding
  // inline constructor that implicitally initializes those managed members,
  // which will overwrite any non-default initializations performed inside
  // of InitComponent() overrides! In this situation, the workaround is to
  // define an explicit constructor that forwards to the base class constructor
  // manually.
  {$DEFINE WORKAROUND_INLINE_CONSTRUCTORS}
{$ENDIF}

{$IFDEF VCL_5_OR_ABOVE}
  {$IFNDEF FPC}
    {$IFNDEF KYLIX}
      {$DEFINE HAS_RemoveFreeNotification}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE HAS_GetObjectProp}
  {$DEFINE HAS_TObjectList}
{$ENDIF}

{$IFDEF VCL_6_OR_ABOVE}
  {$DEFINE HAS_PCardinal}
  {$DEFINE HAS_PByte}
  {$DEFINE HAS_PWord}
  {$DEFINE HAS_PPointer}
  {$DEFINE HAS_TList_Assign}
  {$DEFINE HAS_sLineBreak}
  {$DEFINE HAS_RaiseLastOSError}
  {$DEFINE HAS_SysUtils_IncludeExcludeTrailingPathDelimiter}
  {$DEFINE HAS_SysUtils_DirectoryExists}
  {$DEFINE HAS_UNIT_DateUtils}
  {$DEFINE HAS_UNIT_StrUtils}
  {$DEFINE HAS_UNIT_Types}
  {$DEFINE HAS_TryStrToInt}
  {$DEFINE HAS_TryStrToInt64}
  {$DEFINE HAS_TryEncodeDate}
  {$DEFINE HAS_TryEncodeTime}
  {$DEFINE HAS_ENUM_ELEMENT_VALUES}
  {$IFNDEF FPC}
    {$DEFINE HAS_IInterface}
    {$DEFINE HAS_TSelectionEditor}
    {$DEFINE HAS_TStringList_CaseSensitive}
    {$DEFINE HAS_AcquireExceptionObject}
    {$IFNDEF KYLIX}
      {$DEFINE HAS_DEPRECATED}
      {$DEFINE HAS_SYMBOL_PLATFORM}
      {$DEFINE HAS_UNIT_PLATFORM}
      {$IFNDEF VCL_8_OR_ABOVE}
        // Delphi 6 and 7 have an annoying bug that if a class method is declared as
        // deprecated, the compiler will emit a "symbol is deprecated" warning
        // on the method's implementation!  So we will have to wrap implementations
        // of deprecated methods with {$WARN SYMBOL_DEPRECATED OFF} directives
        // to disable that warning.
        {$DEFINE DEPRECATED_IMPL_BUG}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF DOTNET}
    //Widget defines are omitted in .NET
    {$DEFINE VCL_60_PLUS}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_7_OR_ABOVE}
  {$IFNDEF FPC}
    {$DEFINE HAS_UInt64} // Note: it was just an alias for Int64 until D2006!
    {$DEFINE HAS_NAMED_THREADS}
    {$DEFINE HAS_TStrings_NameValueSeparator}
    {$DEFINE HAS_TStrings_ValueFromIndex}
  {$ENDIF}
  {$DEFINE HAS_TFormatSettings}
  {$DEFINE HAS_PosEx}
  {$IFNDEF VCL_70}
    // not implemented in D7
    {$DEFINE HAS_STATIC_TThread_Queue}
  {$ENDIF}
  {$IFNDEF CIL}
    {$IFNDEF VCL_80}
      // not implemented in D8 or .NET
      {$DEFINE HAS_STATIC_TThread_Synchronize}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFDEF CBUILDER_6}
    {$DEFINE HAS_NAMED_THREADS}
  {$ENDIF}
{$ENDIF}

{$IFNDEF VCL_2005_OR_ABOVE}
  {$IFDEF DCC}
    // InterlockedCompareExchange() was declared in the Windows unit using Pointer
    // parameters until Delphi 2005, when it was switched to Longint parameters
    // instead to match the actual Win32 API declaration.
    {$DEFINE HAS_InterlockedCompareExchange_Pointers}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2006_OR_ABOVE}
  {$DEFINE USE_INLINE}
  {$DEFINE HAS_2PARAM_FileAge}
  {$DEFINE HAS_System_RegisterExpectedMemoryLeak}
  {$IFNDEF FREE_ON_FINAL}
    {$IFNDEF DOTNET}
      {$DEFINE REGISTER_EXPECTED_MEMORY_LEAK}
    {$ENDIF}
  {$ENDIF}
  // In C++Builder 2006 and 2007, UInt64 is emitted as signed __int64 in HPP
  // files instead of as unsigned __int64.  This causes conflicts in overloaded
  // routines that have (U)Int64 parameters.  This was fixed in C++Builder 2009...
  {$IFDEF CBUILDER}
    {$DEFINE BROKEN_UINT64_HPPEMIT}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2007_OR_ABOVE}
  {$IFNDEF CBUILDER_2007}
    // class properties are broken in C++Builder 2007, causing AVs at compile-time
    {$DEFINE HAS_CLASSPROPERTIES}
  {$ENDIF}
  // Native(U)Int exist but are buggy, so do not use them yet
  {.$DEFINE HAS_NativeInt}
  {.$DEFINE HAS_NativeUInt}
  {$DEFINE HAS_StrToInt64Def}
  {$DEFINE HAS_DWORD_PTR}
  {$DEFINE HAS_ULONG_PTR}
  {$DEFINE HAS_ULONGLONG}
  {$DEFINE HAS_PGUID}
  {$DEFINE HAS_PPAnsiChar}
  {$DEFINE HAS_CurrentYear}
  {$IFNDEF DOTNET}
    {$DEFINE HAS_TIMEUNITS}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2009_OR_ABOVE}
  {$IFNDEF DOTNET}
    {$DEFINE STRING_IS_UNICODE}
    {$DEFINE HAS_UnicodeString}
    {$DEFINE HAS_TEncoding}
    {$DEFINE HAS_TCharacter}
    {$DEFINE HAS_InterlockedCompareExchangePointer}
    {$DEFINE HAS_WIDE_TCharArray}
    {$DEFINE HAS_UNIT_AnsiStrings}
    {$DEFINE HAS_PUInt64}
    {$IFDEF VCL_2009}
      // TODO: need to differentiate between RTM and Update 1
      // FmtStr() is broken in RTM but was fixed in Update 1
      {$DEFINE BROKEN_FmtStr}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE HAS_CLASSVARS}
  {$DEFINE HAS_DEPRECATED_MSG}
  {$DEFINE HAS_TBytes}
  // Native(U)Int are still buggy, so do not use them yet
  {.$DEFINE HAS_NativeInt}
  {.$DEFINE HAS_NativeUInt}
  {$DEFINE HAS_Int8}
  {$DEFINE HAS_UInt8}
  {$DEFINE HAS_Int16}
  {$DEFINE HAS_UInt16}
  {$DEFINE HAS_Int32}
  {$DEFINE HAS_UInt32}
  // UInt64 is now emitted as unsigned __int64 in HPP files
  {$IFDEF CBUILDER}
    {$UNDEF BROKEN_UINT64_HPPEMIT}
  {$ENDIF}
  {$IFDEF DCC}
    {$IFDEF WINDOWS}
      // Exception.RaiseOuterException() is only available on Windows at this time
      {$DEFINE HAS_Exception_RaiseOuterException}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2010_OR_ABOVE}
  {$DEFINE HAS_CLASSCONSTRUCTOR}
  {$DEFINE HAS_CLASSDESTRUCTOR}
  {$DEFINE HAS_DELAYLOAD}
  {$DEFINE HAS_TThread_NameThreadForDebugging}
  {$DEFINE DEPRECATED_TThread_SuspendResume}
  // Native(U)Int are finally ok to use now
  {$DEFINE HAS_NativeInt}
  {$DEFINE HAS_NativeUInt}
  {$DEFINE HAS_USHORT}
  {$DEFINE HAS_IOUtils_TPath}
{$ENDIF}

{$IFDEF VCL_XE_OR_ABOVE}
  {$DEFINE HAS_TFormatSettings_Object}
  {$DEFINE HAS_LocaleCharsFromUnicode}
  {$DEFINE HAS_UnicodeFromLocaleChars}
  {$DEFINE HAS_PVOID}
  {$DEFINE HAS_ULONG64}
  {$DEFINE HAS_TEncoding_GetEncoding_ByEncodingName}
  {$IFDEF DCC}
    // Exception.RaiseOuterException() is now available on all platforms
    {$DEFINE HAS_Exception_RaiseOuterException}
  {$ENDIF}
  {$IFNDEF DOTNET}
    {$DEFINE HAS_TInterlocked}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE2_OR_ABOVE}
  {$DEFINE HAS_SIZE_T}
  {$DEFINE HAS_PSIZE_T}
  {$DEFINE HAS_LONG}
  {$DEFINE HAS_ComponentPlatformsAttribute}
  {$DEFINE HAS_ComponentPlatformsAttribute_Win32}
  {$DEFINE HAS_ComponentPlatformsAttribute_Win64}
  {$DEFINE HAS_ComponentPlatformsAttribute_OSX32}
  {$DEFINE HAS_System_ReturnAddress}
  {$DEFINE HAS_DIRECTIVE_WARN_DEFAULT}
{$ENDIF}

{$IFDEF VCL_XE3_OR_ABOVE}
  {$DEFINE HAS_DIRECTIVE_ZEROBASEDSTRINGS}
  {$DEFINE HAS_SysUtils_TStringHelper}
  {$IFDEF NEXTGEN}
    {$DEFINE DCC_NEXTGEN}
    {$DEFINE HAS_MarshaledAString}
    {$DEFINE USE_MARSHALLED_PTRS}
    {$IFDEF AUTOREFCOUNT}
      {$DEFINE USE_OBJECT_ARC}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE4_OR_ABOVE}
  {$DEFINE HAS_AnsiStrings_StrPLCopy}
  {$DEFINE HAS_AnsiStrings_StrLen}
  {$DEFINE HAS_Character_TCharHelper}
  {$DEFINE HAS_ComponentPlatformsAttribute_iOS_Simulator}
  {$DEFINE HAS_ComponentPlatformsAttribute_iOS_Device}
  // technically, these are present in XE4, but they are not used yet
  {.$DEFINE HAS_ComponentPlatformsAttribute_Android}
  {.$DEFINE HAS_ComponentPlatformsAttribute_Linux32}
  {.$DEFINE HAS_ComponentPlatformsAttribute_WinNX32}
{$ENDIF}

{$IFDEF VCL_XE5_OR_ABOVE}
  {$DEFINE HAS_ComponentPlatformsAttribute_Android}
{$ENDIF}

{$IFDEF VCL_XE5_UPDATE2_OR_ABOVE}
  {$DEFINE HAS_DIRECTIVE_HPPEMIT_LINKUNIT}
{$ENDIF}

{$IFDEF VCL_XE7_OR_ABOVE}
  {$DEFINE HAS_TNetEncoding}
{$ENDIF}

{$IFDEF VCL_XE8_OR_ABOVE}
  {$DEFINE HAS_ComponentPlatformsAttribute_iOS_Device32}
  {$DEFINE HAS_ComponentPlatformsAttribute_iOS_Device64}
  // technically, these are present in XE8, but they are not used yet
  {.$DEFINE HAS_ComponentPlatformsAttribute_Linux64}
  {.$DEFINE HAS_ComponentPlatformsAttribute_WinIoT32}
{$ENDIF}

{$IFDEF VCL_10_0_SEATTLE_OR_ABOVE}
  {$IFDEF ANDROID}
    {$DEFINE HAS_TAndroidHelper}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_10_1_BERLIN_OR_ABOVE}
  {$DEFINE HAS_DIRECTIVE_HPPEMIT_LEGACYHPP}
  {$DEFINE HAS_TStrings_AddPair}
{$ENDIF}

{$IFDEF VCL_10_2_TOKYO_OR_ABOVE}
  {.$WARN IMPLICIT_INTEGER_CAST_LOSS OFF}
  {.$WARN IMPLICIT_CONVERSION_LOSS OFF}
  {.$WARN COMBINING_SIGNED_UNSIGNED64 OFF}
  {$DEFINE HAS_STATIC_TThread_ForceQueue}
  // In Delphi 10.2 Tokyo, TThread.ForceQueue() is broken on Android, the
  // passed in procedure is called immediately instead of being delayed!
  {$IFDEF ANDROID}
    {$DEFINE BROKEN_TThread_ForceQueue}
  {$ENDIF}
{$ENDIF}

// Delphi XE+ cross-compiling
{$IFNDEF FPC}
  {$IFDEF POSIX}
    {$IF RTLVersion >= 22.0}
      {$DEFINE UNIX}
      {$UNDEF USE_BASEUNIX}
      {$DEFINE VCL_CROSS_COMPILE}
      {$DEFINE USE_VCL_POSIX}
    {$IFEND}
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IF RTLVersion >= 22.0}
        {$DEFINE VCL_CROSS_COMPILE}
        {$DEFINE USE_VCL_POSIX}
      {$IFEND}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_CROSS_COMPILE}
  {$UNDEF KYLIXCOMPAT}
{$ELSE}
  {$IFDEF KYLIXCOMPAT}
    {$linklib c}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE USE_INLINE}
  {$DEFINE USE_CLASSINLINE}
  {$DEFINE USE_TBitBtn} //use Bit Buttons instead of Buttons
  {$DEFINE FPC_REINTRODUCE_BUG}
  {$DEFINE FPC_CIRCULAR_BUG}
  {$DEFINE NO_REDECLARE}
  {$DEFINE BYTE_COMPARE_SETS}
  {$DEFINE HAS_QWord} // TODO: when was QWord introduced?
  {$DEFINE HAS_PQWord} // TODO: when was PQWord introduced?
  {$IFDEF FPC_2_1_5_OR_ABOVE}
    {$DEFINE HAS_UInt64}
    {.$DEFINE HAS_PUInt64} // TODO: is this defined?
  {$ENDIF}
  {$IFDEF FPC_2_2_0_OR_ABOVE}
    {$DEFINE HAS_InterlockedCompareExchange_Pointers}
  {$ENDIF}
  {$IFDEF FPC_2_2_2_OR_ABOVE}
    {$DEFINE HAS_SharedSuffix}
  {$ENDIF}
  {$IFDEF FPC_2_2_4_OR_ABOVE}
    // size_t and psize_t are only available on Unix systems (FreeBSD, Linux, etc)
    {$IFDEF UNIX}
      {$DEFINE HAS_SIZE_T}
      {$DEFINE HAS_PSIZE_T}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE HAS_PtrInt}
  {$DEFINE HAS_PtrUInt}
  {$DEFINE HAS_PGUID}
  {$DEFINE HAS_LPGUID}
  {$DEFINE HAS_PPAnsiChar}
  {$DEFINE HAS_ENUM_ELEMENT_VALUES}
  {$DEFINE HAS_AcquireExceptionObject} // TODO: when was AcquireExceptionObject introduced?
  {$IFDEF WINDOWS}
    {$DEFINE HAS_ULONG_PTR}
    {.$DEFINE HAS_ULONGLONG} // TODO: is this defined?
  {$ENDIF}
  {$DEFINE HAS_UNIT_ctypes}
  {$DEFINE HAS_sLineBreak}
  {$IFDEF FPC_HAS_UNICODESTRING}
    {$DEFINE HAS_UnicodeString}
  {$ELSE}
    {$IFDEF FPC_2_4_0_OR_ABOVE}
      {$DEFINE HAS_UnicodeString}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF FPC_2_4_4_OR_ABOVE}
    {$DEFINE DEPRECATED_TThread_SuspendResume}
    {$DEFINE HAS_DEPRECATED} // TODO: when was deprecated introduced? Possibly 1.9.x
    {$DEFINE HAS_DEPRECATED_MSG} // TODO: when was message support added? Possibly 2.4.x
    {$DEFINE HAS_STATIC_TThread_Synchronize}
    {$IFNDEF FPC_2_6_2_OR_ABOVE}
      {$DEFINE USE_SEMICOLON_BEFORE_DEPRECATED} // TODO: which earlier versions require a semicolon?
    {$ENDIF}
  {$ENDIF}
  {$IFDEF FPC_2_6_0_OR_ABOVE}
    {$DEFINE HAS_NativeInt}
    {$DEFINE HAS_NativeUInt}
  {$ENDIF}
  {$IFDEF FPC_2_6_2_OR_ABOVE}
    {$DEFINE HAS_Int8}
    {$DEFINE HAS_UInt8}
    {$DEFINE HAS_Int16}
    {$DEFINE HAS_UInt16}
    {$DEFINE HAS_Int32}
    {$DEFINE HAS_UInt32}
  {$ENDIF}
  {$IFDEF FPC_2_6_4_OR_ABOVE}
    {$DEFINE HAS_PInt8}
    {$DEFINE HAS_PUInt8}
    {$DEFINE HAS_PInt16}
    {$DEFINE HAS_PUInt16}
    {$DEFINE HAS_PInt32}
    {$DEFINE HAS_PUInt32}
  {$ENDIF}
  {$IFDEF FPC_3_0_0_OR_ABOVE}
    {$DEFINE HAS_STATIC_TThread_Queue}
  {$ENDIF}
  {$IFDEF FPC_UNICODESTRINGS}
    {$DEFINE STRING_IS_UNICODE}
  {$ENDIF}
  {$IFDEF FPC_3_1_1_OR_ABOVE}
    {$DEFINE HAS_STATIC_TThread_ForceQueue} // requires rev 37359+
  {$ENDIF}
{$ENDIF}

{$IFDEF DOTNET}
  {$DEFINE WIDGET_WINFORMS}
{$ELSE}
  {$DEFINE WIDGET_VCL_LIKE} // LCL included.
  {$DEFINE WIDGET_VCL_LIKE_OR_KYLIX}
  {$IFDEF FPC}
    {$DEFINE WIDGET_LCL}
  {$ELSE}
    {$IFDEF KYLIX}
      {$DEFINE WIDGET_KYLIX}
    {$ELSE}
      {$DEFINE WIDGET_VCL}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// .NET and Delphi 2009+ support UNICODE strings natively!
//
// FreePascal 2.4.0+ supports UnicodeString, but does not map its
// native String type to UnicodeString except when {$MODE DelphiUnicode}
// or {$MODESWITCH UnicodeStrings} is enabled.  However, UNICODE is not
// defined in that mode yet until its RTL has been updated to support
// UnicodeString.  STRING_UNICODE_MISMATCH is defined when the native
// String/Char types do not map to the same types that APIs are expecting
// based on whether UNICODE is defined or not.
//
// NOTE: Do not define UNICODE here.  The compiler defines
// the symbol automatically.
{$IFDEF STRING_IS_UNICODE}
  {$IFNDEF UNICODE}
    {$DEFINE STRING_UNICODE_MISMATCH}
  {$ENDIF}
{$ELSE}
  {$DEFINE STRING_IS_ANSI}
  {$IFDEF UNICODE}
    {$DEFINE STRING_UNICODE_MISMATCH}
  {$ENDIF}
{$ENDIF}

{$IFDEF DCC_NEXTGEN}
  {$DEFINE NO_ANSI_TYPES}
  {.$DEFINE STRING_IS_IMMUTABLE} // Strings are NOT immutable in NEXTGEN yet
  {$IFDEF USE_OBJECT_ARC}
    // TODO: move these to an appropriate section. Not doing this yet because
    // it is a major interface change to switch to Generics and we should
    // maintain backwards compatibility with earlier compilers for the time
    // being. Defining them only here for now because the non-Generic versions
    // of these classes have become deprecated by ARC and so we need to start
    // taking advantage of the Generics versions...
    {$DEFINE HAS_UNIT_Generics_Collections}
    {$DEFINE HAS_UNIT_Generics_Defaults}
    {$DEFINE HAS_GENERICS_TDictionary}
    {$DEFINE HAS_GENERICS_TList}
    {$DEFINE HAS_GENERICS_TObjectList}
    {$DEFINE HAS_GENERICS_TThreadList}
    // TArray<T>.Copy() was introduced in XE7 but was buggy. It was fixed in XE8:
    //
    // RSP-9763 TArray.Copy<T> copies from destination to source for unmanaged types
    // https://quality.embarcadero.com/browse/RSP-9763
    //
    {$IFDEF VCL_XE8_OR_ABOVE}
      {$DEFINE HAS_GENERICS_TArray_Copy}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// TODO: Ansi data types were disabled on mobile platforms in XE3, but
// UTF8String and RawByteString were re-enabled in 10.1 Berlin! What else,
// if anything, was re-enabled to facilitate that?
{$IFDEF NO_ANSI_TYPES}
  {$UNDEF HAS_AnsiString}
  {$UNDEF HAS_AnsiChar}
  {$UNDEF HAS_PAnsiChar}
  {$UNDEF HAS_PPAnsiChar}
  {$UNDEF HAS_UNIT_AnsiStrings}
  {$UNDEF HAS_AnsiStrings_StrPLCopy}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WIN32_OR_WIN64}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE WIN32_OR_WIN64}
{$ENDIF}

{$IFDEF WIN32_OR_WIN64}
  {$DEFINE USE_OPENSSL}
  {$DEFINE USE_ZLIB_UNIT}
  {$IFNDEF DCC_NEXTGEN}
    {$DEFINE USE_SSPI}
    {$IFDEF STRING_IS_UNICODE}
      {$DEFINE SSPI_UNICODE}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF WINCE}
  {$DEFINE USE_OPENSSL}
  // RLebeau: not sure if the above Win32/64 defines also apply to WinCE,
  // so keeping them separate for now...
{$ENDIF}

// High-performance counters are not reliable on multi-core systems, and have
// been known to cause problems with TIdIOHandler.ReadLn() timeouts in Windows
// XP SP3, both 32-bit and 64-bit.  Refer to these discussions for more info:
//
// http://www.virtualdub.org/blog/pivot/entry.php?id=106
// http://blogs.msdn.com/oldnewthing/archive/2008/09/08/8931563.aspx
//
// Do not enable thus unless you know it will work correctly on your systems!
{$IFDEF WINDOWS}
  {.$DEFINE USE_HI_PERF_COUNTER_FOR_TICKS}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE USE_OPENSSL}
  {$DEFINE USE_ZLIB_UNIT}
{$ENDIF}

{$IFDEF MACOS}
  {$DEFINE HAS_getifaddrs}
{$ENDIF}

{$IFDEF LINUX64}
  {$DEFINE HAS_getifaddrs}
{$ENDIF}

{$IFDEF IOS}
  {$DEFINE HAS_getifaddrs}
  {$DEFINE USE_OPENSSL}
  {$IFDEF CPUARM}
    // RLebeau: For iOS devices, OpenSSL cannot be used as an external library,
    // it must be statically linked into the app.  For the iOS simulator, this
    // is not true.  Users who want to use OpenSSL in iOS device apps will need
    // to add the static OpenSSL library to the project and then include the
    // IdSSLOpenSSLHeaders_static unit in their uses clause. It hooks up the
    // statically linked functions for the IdSSLOpenSSLHeaders unit to use...
    {$DEFINE STATICLOAD_OPENSSL}
  {$ENDIF}
{$ENDIF}

{$IFDEF FREEBSD}
  {$DEFINE HAS_getifaddrs}
{$ENDIF}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  {$DEFINE REQUIRES_PROPER_ALIGNMENT}
{$ENDIF}

//
//iconv defines section.
{$DEFINE USE_ICONV_UNIT}
{$DEFINE USE_ICONV_ENC}
{$IFDEF UNIX}
  {$DEFINE USE_ICONV}
  {$IFDEF USE_BASEUNIX}
    {$IFDEF FPC}
      {$UNDEF USE_ICONV_UNIT}
    {$ELSE}
      {$UNDEF USE_ICONV_ENC}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF KYLIXCOMPAT}
    //important!!  Iconv functions are defined in the libc.pas Kylix compatible unit.
    {$UNDEF USE_ICONV_ENC}
    {$UNDEF USE_ICONV_UNIT}
  {$ENDIF}
{$ENDIF}
{$IFDEF NETWARELIBC}
  {$DEFINE USE_ICONV}
  //important!!! iconv functions are defined in the libc.pas Novell Netware header.
  //Do not define USE_ICONV_UNIT
  {$UNDEF USE_ICONV_UNIT}
  {$UNDEF USE_ICONV_ENC}
{$ENDIF}

{$UNDEF USE_SAFELOADLIBRARY}
{$IFDEF WINDOWS}
  {$UNDEF USE_ICONV_ENC}
  {$DEFINE USE_SAFELOADLIBRARY}
{$ENDIF}

{$UNDEF USE_INVALIDATE_MOD_CACHE}
{$UNDEF USE_SAFELOADLIBRARY}
//This must come after the iconv defines because this compiler targets a Unix-like
//operating system.  One key difference is that it does have a TEncoding class.
//If this comes before the ICONV defines, it creates problems.
//This also must go before the THandle size calculations.
{$IFDEF VCL_CROSS_COMPILE}
  {$IFDEF POSIX}
    {$IFNDEF LINUX64}
      {$DEFINE BSD}
    {$ENDIF}
    {$DEFINE USE_SAFELOADLIBRARY}
    {$DEFINE USE_INVALIDATE_MOD_CACHE}
  {$ENDIF}
  //important!!! iconv functions are defined in the libc.pas Novell Netware header.
  //Do not define USE_ICONVUNIT
  {$UNDEF USE_ICONV}
  {$UNDEF USE_ICONV_UNIT}
  {$UNDEF USE_ICONV_ENC}
  {$DEFINE INT_THREAD_PRIORITY}
{$ENDIF}

{$IFNDEF USE_ICONV}
  {$UNDEF USE_ICONV_UNIT}
  {$UNDEF USE_ICONV_ENC}
{$ENDIF}

//IMPORTANT!!!!
//
//Do not remove this!!!  This is to work around a conflict.  In DCC, MACOS
//will mean OS X.  In FreePascal, the DEFINE MACOS means MacIntosh System OS Classic.
{$IFDEF DCC}
  // DCC defines MACOS for both iOS and OS X platforms, need to differentiate
  {$IFDEF MACOS}
    {$IFNDEF IOS}
      {$DEFINE DARWIN}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF MACOS}
    {$DEFINE MACOS_CLASSIC}
  {$ENDIF}
{$ENDIF}

{
BSD 4.4 introduced a minor API change.  sa_family was changed from a 16bit
word to an 8 bit byte and an 8 bit byte field named sa_len was added.
}
//Place this only after DARWIN has been defined for Delphi MACOS
{$IFDEF FREEBSD}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}
{$IFDEF DARWIN}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}
{$IFDEF HAIKU}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}
{$IFDEF MORPHOS}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}

// Do NOT remove these IFDEF's.  They are here because InterlockedExchange
// only handles 32bit values.  Some Operating Systems may have 64bit
// THandles.  This is not always tied to the platform architecture.

{$IFDEF AMIGA}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF ATARI}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF BEOS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF BSD}
  //I think BSD might handle FreeBSD, NetBSD, OpenBSD, and Darwin
  {$IFDEF IOS}
    {$IFDEF CPUARM64}
      {$DEFINE CPU64}
      {$DEFINE THANDLE_64}
    {$ELSE}
      {$IFDEF CPUARM32}
        {$DEFINE CPU32}
      {$ENDIF}
      {$DEFINE THANDLE_32}
    {$ENDIF}
  {$ELSE}
    {$DEFINE THANDLE_32}
  {$ENDIF}
{$ENDIF}
{$IFDEF EMBEDDED}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF EMX}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF GBA}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF GO32}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF HAIKU}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF LINUX}
  {$IFDEF LINUX64}
    {$DEFINE CPU64}
    {$DEFINE THANDLE_64}
  {$ELSE}
    {$IFDEF LINUX32}
      {$DEFINE CPU32}
    {$ENDIF}
    {$DEFINE THANDLE_32}
  {$ENDIF}
{$ENDIF}
{$IFDEF MACOS_CLASSIC}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF MORPHOS}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF NATIVENT} //Native NT for kernel level drivers
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF NDS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF NETWARE}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF NETWARELIBC}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF OS2}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF PALMOS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF SOLARIS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF SYMBIAN}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF WII}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF WATCOM}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}

// end platform specific stuff for THandle size

{$IFDEF THANDLE_CPUBITS}
  {$IFDEF CPU64}
    {$DEFINE THANDLE_64}
  {$ELSE}
    {$DEFINE THANDLE_32}
  {$ENDIF}
{$ENDIF}

{$IFDEF DOTNET}
  {$DEFINE DOTNET_OR_ICONV}
{$ENDIF}
{$IFDEF USE_ICONV}
  {$DEFINE DOTNET_OR_ICONV}
{$ENDIF}

{$UNDEF STREAM_SIZE_64}
{$IFDEF FPC}
  {$DEFINE STREAM_SIZE_64}
{$ELSE}
  {$IFDEF VCL_6_OR_ABOVE}
    {$DEFINE STREAM_SIZE_64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FREE_ON_FINAL}
  {$IFNDEF REGISTER_EXPECTED_MEMORY_LEAK}
    {$IFDEF USE_FASTMM4}
      {$DEFINE REGISTER_EXPECTED_MEMORY_LEAK}
    {$ENDIF}
    {$IFDEF USE_MADEXCEPT}
      {$DEFINE REGISTER_EXPECTED_MEMORY_LEAK}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF REGISTER_EXPECTED_MEMORY_LEAK}
  {$IFDEF DOTNET}
    {$UNDEF REGISTER_EXPECTED_MEMORY_LEAK}
  {$ENDIF}
  {$IFDEF VCL_CROSS_COMPILE}
    // RLebeau: should this be enabled for Windows, at least?
    {$UNDEF REGISTER_EXPECTED_MEMORY_LEAK}
  {$ENDIF}
{$ENDIF}

{
We must determine what the SocketType parameter is for the Socket function.
In DotNET, it's SocketType.  In Kylix and the libc.pas Kylix-compatibility
library, it's a __socket_type.  In BaseUnix, it's a C-type Integer.  In Windows,
it's a LongInt.

}
{$UNDEF SOCKETTYPE_IS_SOCKETTYPE}
{$UNDEF SOCKETTYPE_IS_CINT}
{$UNDEF SOCKETTYPE_IS___SOCKETTYPE}
{$UNDEF SOCKETTYPE_IS_LONGINT}
{$UNDEF SOCKETTYPE_IS_NUMERIC}
{$UNDEF SOCKET_LEN_IS_socklen_t}
{$IFDEF DOTNET}
  {$DEFINE SOCKETTYPE_IS_SOCKETTYPE}
{$ENDIF}
{$IFDEF USE_BASEUNIX}
  {$DEFINE SOCKETTYPE_IS_CINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}
{$IFDEF KYLIXCOMPAT}
  {$DEFINE SOCKETTYPE_IS___SOCKETTYPE}
{$ENDIF}
{$IFDEF USE_VCL_POSIX}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKET_LEN_IS_socklen_t}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}
{$IFDEF OS2}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}
{$IFDEF NETWARE}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}

{Take advantage of some TCP features specific to some stacks.
They work somewhat similarly but there's a key difference.
In Linux, TCP_CORK is turned on to send fixed packet sizes and
when turned-off (uncorked), any remaining data is sent. With
TCP_NOPUSH, this might not happen and remaining data is only sent
before disconnect.  TCP_KEEPIDLE and TCP_KEEPINTVL so the IFDEF LINUX and IFDEF
SOLARIS instead of IFDEF UNIX is not an error, it's deliberate.}
{$UNDEF HAS_TCP_NOPUSH}
{$UNDEF HAS_TCP_CORK}
{$UNDEF HAS_TCP_KEEPIDLE}
{$UNDEF HAS_TCP_KEEPINTVL}
{$UNDEF HAS_SOCKET_NOSIGPIPE}
{$IFDEF BSD}
  {$DEFINE HAS_TCP_NOPUSH}
{$ENDIF}
{$IFDEF HAIKU}
  {$DEFINE HAS_TCP_NOPUSH}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE HAS_TCP_CORK}
  {$DEFINE HAS_TCP_KEEPIDLE}
  {$DEFINE HAS_TCP_KEEPINTVL}
{$ENDIF}
{$IFDEF SOLARIS}
  {$DEFINE HAS_TCP_CORK}
{$ENDIF}
{$IFDEF NETBSD}
  {$DEFINE HAS_TCP_CORK}
  {$DEFINE HAS_TCP_KEEPIDLE}
  {$DEFINE HAS_TCP_KEEPINTVL}
{$ENDIF}
{$IFDEF USE_VCL_POSIX}
  // TODO: which platforms actually have SO_NOSIGPIPE available?
  {$DEFINE HAS_SOCKET_NOSIGPIPE}
  {$IFDEF ANDROID}
    {$UNDEF HAS_SOCKET_NOSIGPIPE}
  {$ENDIF}
  {$IFDEF LINUX}
    {$UNDEF HAS_SOCKET_NOSIGPIPE}
  {$ENDIF}
{$ENDIF}
{end Unix OS specific stuff}
{$IFDEF DEBUG}
  {$UNDEF USE_INLINE}
{$ENDIF}

// RLebeau 5/24/2015: In C++Builder 2006 and 2007, UInt64 is emitted as
// signed __int64 in HPP files instead of as unsigned __int64.  This causes
// conflicts in overloaded routines that have (U)Int64 parameters.  This
// was fixed in C++Builder 2009.  For compilers that do not have a native
// UInt64 type, or for C++Builder 2006/2007, let's define a record type
// that can hold UInt64 values...
{$IFDEF HAS_UInt64}
  {$IFDEF BROKEN_UINT64_HPPEMIT}
    {$DEFINE TIdUInt64_HAS_QuadPart}
  {$ENDIF}
{$ELSE}
  {$IFNDEF HAS_QWord}
    {$DEFINE TIdUInt64_HAS_QuadPart}
  {$ENDIF}
{$ENDIF}

// RLebeau 9/5/2013: it would take a lot of work to re-write Indy to support
// both 0-based and 1-based string indexing, so we'll just turn off 0-based
// indexing for now...
{$IFDEF HAS_DIRECTIVE_ZEROBASEDSTRINGS}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}