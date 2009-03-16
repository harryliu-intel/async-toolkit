(* $Id$ *)

MODULE SchemeProfiler;
IMPORT SchemeProcedure;
IMPORT Uresource;

PROCEDURE EnterProcedure(p : SchemeProcedure.T) =
  BEGIN
    LOCK mu DO
      IF enabled = TRUE THEN
        VAR
          r : struct_rusage;
          rusage_retval := Uresource.getrusage(RUSAGE_SELF,r);
          (* RUSAGE_CHILDREN too? *)
        BEGIN
        END
      END
    END
  END EnterProcedure;

VAR mu := NEW(MUTEX);
    enabled := FALSE;

PROCEDURE Enable() = BEGIN LOCK mu DO enabled := TRUE END END Enable;

PROCEDURE Disable() = BEGIN LOCK mu DO enabled := FALSE END END Disable;

PROCEDURE TopN(n : CARDINAL) : REF ARRAY OF Stats =
  BEGIN RETURN NIL END TopN;

BEGIN END SchemeProfiler.
