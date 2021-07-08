INTERFACE DataBlock;
IMPORT Wr, Rd;

PROCEDURE WriteData(wr        : Wr.T;
                    tag       : CARDINAL; (* written if nonzero *)
                    READONLY block     : ARRAY OF LONGREAL)
  RAISES { Wr.Failure };

PROCEDURE DataCount(rd : Rd.T; tag : CARDINAL) : CARDINAL RAISES { Rd.Failure };
  (* return # of timesteps in file
     for now only valid for tag = 0 *)
  
PROCEDURE ReadData(rd        : Rd.T;
                   tag       : CARDINAL; (* must match if nonzero *)
                   VAR data  : ARRAY OF LONGREAL;
                   fn        : TEXT (* for debug *) ) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile };
  (* error if the block is longer than the data array 
     returns # of items read 
     returns 0 if the tag doesn't match
     raises Rd.EndOfFile if at EOF
  *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    (* an object for reading a set of non-time data out of a file *)
    init(rd : Rd.T; maxCount : CARDINAL; fn : TEXT (* for debug *)) : T
      RAISES { Rd.Failure } ;

    haveTag(tag : CARDINAL) : BOOLEAN;

    readData(tag : CARDINAL; VAR data : ARRAY OF LONGREAL) : CARDINAL;
  END;

CONST Brand = "DataBlock";
    
PROCEDURE DebugTraverse(rd : Rd.T; fn : TEXT) RAISES { Rd.Failure };

END DataBlock.
  

