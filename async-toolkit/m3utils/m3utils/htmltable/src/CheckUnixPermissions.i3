INTERFACE CheckUnixPermissions;
IMPORT Pathname;

PROCEDURE Check(path : Pathname.T) : BOOLEAN;

TYPE
  Reason = RECORD
    result   : BOOLEAN;
    failPath : Pathname.T;
  END;
  
PROCEDURE GetReason(path : Pathname.T) : Reason;

END CheckUnixPermissions.
