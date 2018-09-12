INTERFACE CsrOp;
IMPORT Word;
IMPORT CompAddr;
IMPORT CompRange;

TYPE
  D = Word.T;

  Address = Word.T;

  RW = { R, W };
  
  T = RECORD
    rw     :   RW;
    (* is this a read or a write *)
    
    at     :   Address;
    (* word address *)

    data   :   REF ARRAY OF D := NIL;
    (* data in/out *)

    single : D;
    (* this is valid if data is NIL, otherwise not used *)

    hi : CompAddr.T;
    (* computed from the above *)

    doStruct : BOOLEAN;
    (* descend into main struct or not *)
    
    fv :   [0..Base-1];
    lv :   [-1..Base-1];
    (* in first word: first valid data 
       in last word: last valid data 
       lv=-1 is for the empty case fv=0,lv=-1
    *)

    (* do we need/want a full bitmask? probably not? *)

    origin : Origin;
    (* who initiated the write? *)
  END;

  Origin = { Hardware, Software };

CONST RWnames = ARRAY RW OF TEXT { "R", "W" };

CONST OriginNames = ARRAY Origin OF TEXT { "Hardware", "Software" };

PROCEDURE Format(READONLY t : T) : TEXT;
  
CONST Base = BITSIZE(D);

CONST Brand = "CsrOp";

PROCEDURE MakeRead     (at           : CompAddr.T;
                        bits         : [0..BITSIZE(Word.T)];
                        origin       := Origin.Hardware) : T;
  (* make a read at a given CompAddr with a given width in bits *)

PROCEDURE GetReadResult(op : T) : Word.T;
  (* get the result of executing a read as above,
     result will be right-shifted in the Word *)
  

PROCEDURE MakeWrite    (at           : CompAddr.T;
                        bits         : [0..BITSIZE(Word.T)];
                        val          : Word.T;
                        doStruct     := TRUE;
                        origin       := Origin.Hardware) : T;

PROCEDURE MakeWideWrite(at           : CompAddr.T;
                        READONLY val : ARRAY OF [0..1];
                        doStruct     := TRUE;
                        origin       := Origin.Hardware) : T;

PROCEDURE Hi(t : T) : CompAddr.T;
  (* return index of first bit not written *)

PROCEDURE LowAddr(t : T) : CompAddr.T;

PROCEDURE DoField(VAR op : T; d : Word.T; READONLY a : CompRange.T) : Word.T;
  (* given a field residing at a, containing d, execute op against
     the field and return the post-write contents of the field *)

PROCEDURE DoWideField(VAR op : T; VAR d : ARRAY OF [0..1]; a : CompRange.T);
  (* given a field residing at a, containing d, execute op against
     the field and update the field accordingly *)
  
END CsrOp.
    
