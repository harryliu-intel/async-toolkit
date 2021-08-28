INTERFACE AplotFile;

(* Modula-3 interface to data files for Andrew Lines's "aplot" plotting program

   Node information is not handled by this interface.  Nodes are identified
   by IDs. ID 0 is reserved. 

   Copyright (c) Intel Corp., 2021.

   Author: Mika Nystrom <mika.nystroem@intel.com>
   August, 2021
*)

IMPORT AplotHeader;
IMPORT AplotNode;
IMPORT OSError, Rd, Wr;

(* 
   multiple readers operating on a single File --- OK

   multiple writers operating on a single file --- ???
*)

(* check nomenclature : timestep-major vs. node-major *)

TYPE
  T = OBJECT METHODS
    init(format : AplotHeader.Format;
         nodes  : CARDINAL;
         steps  : CARDINAL (* can be zero for timestep-major formats *)) : T;

    openPath(path : Pathname.T) RAISES { OSError.E, Rd.Failure };

    getHeader() : AplotHeader.T;

    getNode(index : AplotNodeIndex) : AplotNode.T RAISES { Rd.Failure };

    updateHeader(header : AplotHeader.T) RAISES { Wr.Failure };

    rewind();
    (* not here? *)
    
    writeNextNode(node : AplotNode.T) RAISES { Wr.Failure };
    (* ??? *)

    getWriter() : AplotWriter.T;

    getReader() : AplotReader.T;
    
    close() RAISES { Rd.Failure, Wr.Failure };
  END;

  Default <: T; (* default implementation of the above interface *)

CONST Brand = "AplotFile";

END AplotFile.
    
    
    
