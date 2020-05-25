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
  Rev 1.5    1/29/2004 8:54:30 AM  JPMugaas
  Removed myself from the distribution Team Chairperson entry as I am resigning
  from that role.

  Rev 1.4    10/15/2003 10:11:46 PM  DSiders
  Added resource string for About box credits.
  Corrected spelling error in comments.

  Rev 1.3    6/16/2003 12:01:44 PM  JPMugaas
  Updated copyright to say 2003.

  Rev 1.2    6/8/2003 05:46:58 AM  JPMugaas
  The kitchen sink has now been implemented.

  Rev 1.1    1/15/2003 08:03:56 AM  JPMugaas
  Updated with new website address.

  Rev 1.0    11/13/2002 08:43:24 AM  JPMugaas
}

unit IdDsnCoreResourceStrings;

{
  Note:  This unit is for resource strings that are used in the Core Design-Time
  package and NOT any design-time packages.  This is to prevent design-time
  resource strings from being linked into the Run-Time only package.
}

interface

{$I IdCompilerDefines.inc}

const
  IndyPitCrew = 'Kudzu (Chad Z. Hower)'#13#10'and the Indy Pit Crew';

resourcestring
  { About Box stuff }
  RSAAboutFormCaption = 'About';
  RSAAboutBoxCompName = 'Internet Direct (Indy)';
  RSAAboutMenuItemName = 'About Internet &Direct (Indy) %s...';

  RSAAboutBoxVersion = 'Version %s';
  RSAAboutBoxCopyright = 'Copyright (c) 1993 - 2017'#13#10 + IndyPitCrew;
  RSAAboutBoxTitle1 = 'INDY';
  RSAAboutBoxTitle2 = 'Internet Direct';
  RSAAboutBoxLicences = 'Indy Modified BSD License'+#13#10+'Indy MPL License';
  RSAAboutBoxBuiltFor = 'Indy.Sockets (%s)';

  RSAAboutBoxDescription = 'Internet Direct is an open source library which supports '
                          + 'clients and servers of TCP, UDP and RAW sockets as well '
                          + 'as over 100 higher level protocols.';

  RSAAboutBoxPleaseVisit = 'For the latest updates and information please visit:';
  RSAAboutBoxIndyWebsite = 'http://www.indyproject.org/';    {Do not Localize}

  RSAAboutCreditsCoordinator = 'Project Coordinator';
  RSAAboutCreditsCoCordinator = 'Project Co-Coordinator';
  RSAAboutCreditsDocumentation = 'Documentation Coordinator';
  RSAAboutCreditsDemos = 'Demos Coordinator';
  RSAAboutCreditsRetiredPast = 'Retired/Past Contributors';
  RSAAboutCreditsIndyCrew = 'The Indy Pit Crew';

  RSAAboutKitchenSink = IndyPitCrew+#10#13+'present the'#10#13'Kitchen Sink';

  {Binding Editor stuff}
  {
  Note to translators - Please Read!!!

  For all the constants except RSBindingFormCaption, there may be an
  & symbol before a letter or number.  This is rendered as that character being
  underlined.  In addition, the character after the & symbol along with the ALT
  key enables a user to move to that control.  Since these are on one form, be
  careful to ensure that the same letter or number does not have a & before it
  in more than one string, otherwise an ALT key sequence will be broken.
  }
  RSBindingFormCaption = 'Binding Editor';
  RSBindingNewCaption  = '&New';
  RSBindingDeleteCaption = '&Delete';
  //RSBindingAddCaption = '&Add';
  RSBindingRemoveCaption = '&Remove';
  RSBindingLabelBindings = '&Bindings';
  RSBindingHostnameLabel = '&IP Address';
  RSBindingPortLabel = '&Port';
  RSBindingIPVerLabel = 'IP &Version';
  RSBindingIPV4Item = 'IPv&4 (127.0.0.1)';
  RSBindingIPV6Item = 'IPv&6 (::1)';
  {Design time SASLList Hints}
  RSADlgSLMoveUp = 'Move Up';
  RSADlgSLMoveDown = 'Move Down';
  RSADlgSLAdd = 'Add';
  RSADlgSLRemove = 'Remove';
  //Caption that uses format with component name
  RSADlgSLCaption = 'Editing SASL List for %s';
  RSADlgSLAvailable = '&Available';
  RSADlgSLAssigned = 'A&ssigned (tried in order listed)';
  {Note that the Ampersand indicates an ALT keyboard sequence}
  RSADlgSLEditList = 'Edit &List';
  //Display item constants
  RSBindingAll = 'All'; //all IP addresses
  RSBindingAny = 'Any'; //any port

  {Standard dialog stock strings}
  RSOk       = 'OK';
  RSCancel   = 'Cancel';
  RSHelp     = '&Help';

    // IdRegister
  RSRegIndyClients = 'Indy Clients';
  RSRegIndyServers = 'Indy Servers';
  RSRegIndyIntercepts = 'Indy Intercepts';
  RSRegIndyIOHandlers = 'Indy I/O Handlers';
  RSRegIndyMisc = 'Indy Misc';
{$IFDEF FPC}
  CoreSuffix = ' - Core';
{$ENDIF}

implementation

end.
