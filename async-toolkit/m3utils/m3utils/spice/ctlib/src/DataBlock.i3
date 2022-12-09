INTERFACE DataBlock;
IMPORT Wr, Rd;

(* 
   this is the interface to the code that writes the temp files in ct.work

   Author : mika.nystroem@intel.com

   the basic format for WriteData/ReadData is:

   if tag = 0 

   component     bits
   ----------------------
   block         variable

   Note that blocks tagged zero are generally written to their own separate
   file.  Such blocks generally represent the TIME independent variable in
   simulations.

   else (tag # 0)

   component     bits
   ----------------------
   tag           32
   threadId      32
   bytes in blk  32
   block         variable

   The idea is that numerous tagged blocks can be written to a single file.
   The ReadData procedure will hop through the file looking for the block
   that is tagged as requested and return that block to the caller.

*)

PROCEDURE WriteData(wr                 : Wr.T;
                    tag                : CARDINAL; (* written if nonzero *)
                    READONLY block     : ARRAY OF LONGREAL)
  RAISES { Wr.Failure };

PROCEDURE WriteCompressed(wr           : Wr.T;
                          tag          : CARDINAL; (* m.b. # 0 *)
                          data         : TEXT)
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

PROCEDURE ReadCompressed(rd        : Rd.T;
                         tag       : CARDINAL; (* must match if nonzero *)
                         fn        : TEXT (* for debug *) ) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile };
  (* returns NIL if the tag doesn't match
     raises Rd.EndOfFile if at EOF
  *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    (* an object for reading a set of non-time data out of a file *)
    init(rd       : Rd.T;
         maxCount : CARDINAL;
         fn       : TEXT (* for debug *)) : T
      RAISES { Rd.Failure } ;

    haveTag(tag : CARDINAL) : BOOLEAN;

    blockType(tag : CARDINAL) : BlockType;

    readData(tag : CARDINAL; VAR data : ARRAY OF LONGREAL) : CARDINAL;

    readCompressed(tag : CARDINAL) : TEXT;
  END;

  BlockType = { Array, Compressed };

CONST Brand = "DataBlock";
    
PROCEDURE DebugTraverse(rd : Rd.T; fn : TEXT) RAISES { Rd.Failure };

END DataBlock.
  

