{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  116136: IdAssemblyInfo.pas 
{
{   Rev 1.1    3/3/2005 7:45:46 PM  JPMugaas
{ Various fixes.
}
{
{   Rev 1.0    3/3/2005 4:45:30 PM  JPMugaas
{ Version info templates for some files.
}
{
{   Rev 1.0    2004.02.03 2:40:36 PM  czhower
{ Move
}
{
{   Rev 1.0    2004.01.20 12:27:00 AM  czhower
{ Initial checkin
}
unit IdAssemblyInfo;

interface

uses
  System.Reflection, System.Runtime.CompilerServices;

//
// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly: AssemblyTitle('Indy')]
[assembly: AssemblyDescription('Internet Direct (Indy) 10.6.2 for Visual Studio .NET')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('Chad Z. Hower a.k.a Kudzu and the Indy Pit Crew')]
[assembly: AssemblyProduct('Indy for Microsoft .NET Framework')]
[assembly: AssemblyCopyright('Copyright © 1993 - 2015 Chad Z. Hower a.k.a Kudzu and the Indy Pit Crew')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

//
// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Revision and Build Numbers
// by using the '*' as shown below:

[assembly: AssemblyVersion('10.6.2.*')]

//
// In order to sign your assembly you must specify a key to use. Refer to the
// Microsoft .NET Framework documentation for more information on assembly signing.
//
// Use the attributes below to control which key is used for signing.
//
// Notes:
//   (*) If no key is specified, the assembly is not signed.
//   (*) KeyName refers to a key that has been installed in the Crypto Service
//       Provider (CSP) on your machine. KeyFile refers to a file which contains
//       a key.
//   (*) If the KeyFile and the KeyName values are both specified, the
//       following processing occurs:
//       (1) If the KeyName can be found in the CSP, that key is used.
//       (2) If the KeyName does not exist and the KeyFile does exist, the key
//           in the KeyFile is installed into the CSP and used.
//   (*) In order to create a KeyFile, you can use the sn.exe (Strong Name) utility.
//       When specifying the KeyFile, the location of the KeyFile should be
//       relative to the project output directory which is
//       %Project Directory%\bin\<configuration>. For example, if your KeyFile is
//       located in the project directory, you would specify the AssemblyKeyFile
//       attribute as [assembly: AssemblyKeyFile('..\\..\\mykey.snk')]
//   (*) Delay Signing is an advanced option - see the Microsoft .NET Framework
//       documentation for more information on this.
//
[assembly: AssemblyDelaySignAttribute(true)]
[assembly: AssemblyKeyFileAttribute('Indy.snk')]
[assembly: AssemblyKeyName('')]

implementation

end.
