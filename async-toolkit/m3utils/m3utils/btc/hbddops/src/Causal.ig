(* $Id$ *)

GENERIC INTERFACE Causal(Elem, ElemSet, ElemList);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init        () : T;

    isSource    (e : Elem.T; VAR at : LONGREAL) : BOOLEAN;  (* override *)
    predecessors(e : Elem.T) : ElemSet.T;                   (* override *)
    delay       (pred, succ : Elem.T) : LONGREAL;           (* override *)

    (* computes *)
    last        (VAR at : Elem.T) : LONGREAL;
    time        (at : Elem.T) : LONGREAL;
    addDependency(pred, succ : Elem.T);
    delDependency(pred, succ : Elem.T);
    changeDelay  (pred, succ : Elem.T; dly : LONGREAL);
  END;

CONST Brand = "Causal(" & Elem.Brand & ")";

END Causal.
