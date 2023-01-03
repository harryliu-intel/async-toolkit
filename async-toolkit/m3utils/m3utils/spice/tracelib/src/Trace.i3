INTERFACE Trace;
IMPORT Pathname;
IMPORT OSError, Rd;
IMPORT TextSet;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root : Pathname.T) : T
      RAISES { OSError.E, Rd.Failure, Rd.EndOfFile };
    (* files <root>.trace and <root>.names are those read *)
  
    getNodeIdx(node : TEXT; VAR idx : CARDINAL) : BOOLEAN;

    getSteps() : CARDINAL;

    getNodes() : CARDINAL;
    (* node 0 is always TIME and is included in the count *)

    (* there is really no concept of a fixed timestep in a trace file *)
    
    getTimeData(VAR timea : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, Rd.EndOfFile } ;
    (* = .getNodeArray(0, timea) *)

    getNodeData(idx : CARDINAL; VAR arr : ARRAY OF LONGREAL)
    RAISES { Rd.Failure, Rd.EndOfFile } ;

    getCanonicalName(idx : CARDINAL) : TEXT;

    getAliases(idx : CARDINAL) : TextSet.T;

    allNames() : TextSet.T;

    close() RAISES { Rd.Failure };
  END;

CONST Brand = "Trace";

END Trace.
