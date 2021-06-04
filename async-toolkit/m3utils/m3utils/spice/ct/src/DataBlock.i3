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
                   VAR data  : ARRAY OF LONGREAL) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile };
  (* error if the block is longer than the data array 
     returns # of items read 
  *)

END DataBlock.
  

