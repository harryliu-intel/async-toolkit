INTERFACE CompRange;
IMPORT CompAddr;

TYPE
  T = RECORD
    pos : CompAddr.T;
    wid : CompAddr.T; 
  END;

CONST Brand = "CompRange";

TYPE
  Prop = { Regwidth, Alignment, Accesswidth, Addressing };
  
CONST
  PropNames = ARRAY Prop OF TEXT {
    "regwidth",
    "alignment",
    "accesswidth",
    "addressing"
  };

   DefProp = ARRAY Prop OF TEXT {
    "32",
    "CompAddr.Unspecified",
    "32",
    "CompAddr.Addressing.Regalign"
  };
    
   PropType = ARRAY Prop OF TEXT {
     "CARDINAL",
     "CARDINAL",
     "CARDINAL",
     "CompAddr.Addressing"
   };
   
PROCEDURE PlaceReg(at          : CompAddr.T;          (* first free place *)
                   regwidth    : CARDINAL;
                   alignment   : CARDINAL;
                   accesswidth : CARDINAL;
                   addressing  : CompAddr.Addressing;
                   ) : T;
  (* given that the first free bit is at           at          [T]
     alignment                                     alignment   [bytes]
     accesswidth                                   accesswidth [bits]
     under addressing mode                         addressing  [Addressing]

     the spec of the structure layout is returned.
     the LSB of the structure is at res.pos
     the next free bit is at 
     CompAddr.Plus(res.pos,res.wid)
  *)

PROCEDURE Hi(x : T) : CompAddr.T;
  (* the next free bit *)

PROCEDURE MakeField(at : CompAddr.T; width : CARDINAL) : T;

PROCEDURE Format(READONLY a : T) : TEXT;

TYPE
  Monotonic <: PubMonotonic;

  PubMonotonic = OBJECT METHODS
    init() : Monotonic;

    increase(from : CompAddr.T; to : T) : CompAddr.T;
    (* record the new record in to *)

    isok() : BOOLEAN;
    (* return FALSE if los are nonmonotonic *)

    indexArr() : REF ARRAY OF CARDINAL;
    (* return array of indices of elements, were they sorted *)

    setRange(VAR min, max : CompAddr.T);
  END;

PROCEDURE From2(lo, lim : CompAddr.T) : T;
  
END CompRange.
