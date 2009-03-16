(* $Id$ *)

INTERFACE SchemeProfiler;
IMPORT SchemeProcedure, Time;

PROCEDURE EnterProcedure(p : SchemeProcedure.T);
  (* no point in having a LeaveProcedure, owing to tail recursion *)
  
PROCEDURE Enable();

PROCEDURE Disable();

PROCEDURE TopN(n : CARDINAL) : REF ARRAY OF Stats;

TYPE
  Stats = RECORD
    name : TEXT;
    wall : Time.T;
    cpu : Time.T;
  END;

CONST Brand = "SchemeProfiler";

END SchemeProfiler.
