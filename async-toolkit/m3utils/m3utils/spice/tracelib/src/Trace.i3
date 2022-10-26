INTERFACE Trace;
IMPORT Pathname;
IMPORT OSError, Rd;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(root : Pathname.T) : T
      RAISES { OSError.E, Rd.Failure, Rd.EndOfFile };
    (* files <root>.trace and <root>.names are those read *)
  
    getNodeIdx(node : TEXT; VAR idx : CARDINAL) : BOOLEAN;

    getSteps() : CARDINAL;

    getTimeStep() : LONGREAL;
    
    getTimeData(VAR timea : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, Rd.EndOfFile } ;
    (* = .getNodeArray(0, timea) *)

    getNodeData(idx : CARDINAL; VAR arr : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, Rd.EndOfFile } ;

    close() RAISES { Rd.Failure };
  END;

CONST Brand = "Trace";

END Trace.
