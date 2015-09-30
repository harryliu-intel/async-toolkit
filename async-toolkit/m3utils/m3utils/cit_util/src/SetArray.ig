(* $Id: SetArray.ig,v 1.1 2007/06/24 22:17:49 mika Exp $ *)

GENERIC INTERFACE SetArray(Elem, ElemSet);

TYPE 
  Public = ElemSet.T OBJECT METHODS
    init(sizeHint : CARDINAL := 0; 
         compare : Comparer := NIL;
         compareR : ComparerR := NIL) : T;
  END;

  T <: Public;

  Iterator <: ElemSet.Iterator;

  Comparer = PROCEDURE (a, b : Elem.T) : [-1..1];
  ComparerR = PROCEDURE (READONLY a, b : Elem.T) : [-1..1];

END SetArray.
