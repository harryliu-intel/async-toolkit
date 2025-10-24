(* $Id: TeXHalign.i3,v 1.3 2006/02/26 22:46:30 mika Exp $ *)

INTERFACE TeXHalign;
IMPORT CardSet;

TYPE 
  Point = RECORD row, col : [-1..LAST(CARDINAL)] END; 
  (* N.B. matrix addressing convention;
     -1 value is required for boundary of cells in row and col 0 *)

  T <: Public;

  Public = OBJECT METHODS
    init(height, width : CARDINAL) : T;
    put(p : Point; what : TEXT);
    addRule(betweenNW, betweenSE : Point);
    (* add a rule between the first cell, directly to the north or west,
       and the second cell, directly to the south or east

       N.B. the implementation may insert a rule between entire rows if
            it cannot figure out how to make a shortened rule between
            a few cells only.
     *)

    makeColumnGang(cols : CardSet.T); (* is this the right data type? *)
    (* in any case, the referenced columns will be constrained to be the
       same width (the width of the widest one's text plus quads) *)
    
    makeTeX() : TEXT;

    setJustification(column : CARDINAL; justification : Justification);
    (* default justification is Justification.Right *)
  END;

  Justification = { Right, Center, Left };

CONST Brand = "TeXHalign";

END TeXHalign.
