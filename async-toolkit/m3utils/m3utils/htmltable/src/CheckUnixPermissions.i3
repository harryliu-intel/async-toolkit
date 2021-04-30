INTERFACE CheckUnixPermissions;
IMPORT Pathname;
IMPORT AtomList;

(* note how this works.

   A bad pathname can raise an exception.

   But a non-existent user will simply result in a denial, unless every
   arc is world-accessible.

   If the reading process cannot read the arcs itself, we get a denial
   at that point.
*)

PROCEDURE Check(user : TEXT; path : Pathname.T) : BOOLEAN
  RAISES { Pathname.Invalid };

TYPE
  Reason = RECORD
    result   : BOOLEAN;
    failPath : Pathname.T;
    osError  : AtomList.T;  (* this is filled in if we hit OSError.E *)
  END;
  
PROCEDURE GetReason(user : TEXT; path : Pathname.T) : Reason
  RAISES { Pathname.Invalid };

END CheckUnixPermissions.
