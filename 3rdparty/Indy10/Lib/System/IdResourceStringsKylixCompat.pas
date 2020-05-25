unit IdResourceStringsKylixCompat;

interface

resourcestring
  RSReverseResolveError = 'Error resolving Address %s: %s (%d)'; { address, errorstring, errornumber }

  RSStackTRY_AGAIN = 'Non-authoritative response (try again or check DNS setup).';
  RSStackNO_RECOVERY = 'Non-recoverable errors: FORMERR, REFUSED, NOTIMP.';
  RSStackNO_DATA = 'Valid name, no data record (check DNS setup).';

implementation

end.
