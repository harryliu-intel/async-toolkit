INTERFACE Trace;

(* 

   Interface for reading aspice-style .trace files
   Also supports .ztrace for the new, compressed formats.

*)

IMPORT Pathname;
IMPORT OSError, Rd;
IMPORT TextSet;
IMPORT TraceFile;

TYPE
  T <: Public;

  NodeId = CARDINAL;
  
  Public = OBJECT METHODS
    init(root : Pathname.T) : T
      RAISES { OSError.E, Rd.Failure, Rd.EndOfFile, TraceFile.FormatError };
    (* files <root>.trace or <root>.ztrace and <root>.names are those read *)
  
    getNodeIdx(node : TEXT; VAR idx : NodeId) : BOOLEAN;
    (* look up a name in the names file.
       If it does exist, return the index of the node in the trace file
       in idx and TRUE as the return vale.
       If it does not exist, return FALSE as the return value *)

    getSteps() : CARDINAL;
    (* 
       the number of timesteps for each node.
       Note that the format has the same number of timesteps for
       every node.

       The timesteps are not necessarily equally spaced, but most
       sources as of the date of writing will generate equal timesteps
    *)

    getNodes() : CARDINAL;
    (* node 0 is always TIME and is included in the count *)

    getTimeData(VAR timea : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, Rd.EndOfFile } ;
    (* = .getNodeArray(0, timea) *)

    getNodeData(idx : NodeId; VAR arr : ARRAY OF LONGREAL)
      RAISES { Rd.Failure, Rd.EndOfFile } ;

    getCanonicalName(idx : NodeId) : TEXT;
    (* this is just the first name in the line in the .names file *)

    getAliases(idx : NodeId) : TextSet.T;

    allNames() : TextSet.T;
    (* return a set of all aliases of all nodes *)

    close() RAISES { Rd.Failure };
    (* close the internal reader *)

    sharedTime() : REF ARRAY OF LONGREAL RAISES { Rd.EndOfFile, Rd.Failure } ;
    (* allocate and return a handle to a shared time (s.b. treated as R/O) *)
  END;

CONST TimeId = 0;

CONST Brand = "Trace";

END Trace.
