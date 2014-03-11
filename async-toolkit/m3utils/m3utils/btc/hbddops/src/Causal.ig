(* $Id$ *)

GENERIC INTERFACE Causal(Elem, ElemSet);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init        () : T;

    isSource    (e : Elem.T; VAR at : LONGREAL) : BOOLEAN;  (* override *)
    predecessors(e : Elem.T) : ElemSet.T;                   (* override *)
    delay       (pred, succ : Elem.T) : LONGREAL;           (* override *)
    debugFmt    (e : Elem.T) : TEXT;                        (* may override *)

    (* computes *)
    last        (VAR at : Elem.T) : LONGREAL;
    time        (at : Elem.T) : LONGREAL;
    addDependency(pred, succ : Elem.T                ; sync := TRUE);
    delDependency(pred, succ : Elem.T                ; sync := TRUE);
    changeDelay  (pred, succ : Elem.T; dly : LONGREAL; sync := TRUE);

    successors    (e : Elem.T) : ElemSet.T (* CONST *);

    sync();
  END;

CONST Brand = "Causal(" & Elem.Brand & ")";

END Causal.
