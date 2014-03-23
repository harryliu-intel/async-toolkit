(* $Id$ *)

GENERIC INTERFACE Causal(Elem, ElemSet, ElemSeq);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init        () : T;

    debugFmt    (e : Elem.T) : TEXT;                        (* may override *)

    (* create elements -- not needed? *)
    (* createElement(e : Elem.T); *)

    (* modify *)
    addDependency(pred, succ : Elem.T                ; sync := TRUE);
    delDependency(pred, succ : Elem.T                ; sync := TRUE);
    setDelay  (pred, succ : Elem.T; dly : LONGREAL   ; sync := TRUE);

    setSource (e : Elem.T; at : LONGREAL);

    sync(); (* either sync = TRUE above or call this before computing below *)

    (* computes *)
    last             (VAR at : Elem.T) : LONGREAL;
    time             (at : Elem.T)     : LONGREAL;
    findCriticalInput(e : Elem.T; VAR crit : Elem.T) : BOOLEAN;

    (* queries *)
    getDelay(pred, succ : Elem.T) : LONGREAL;
    successors       (e : Elem.T) : ElemSet.T (* CONST *);
    predecessors  (e : Elem.T)    : ElemSet.T (* CONST *);

    topoSort (syncOnly := FALSE)  : ElemSeq.T;

  END;

CONST Brand = "Causal(" & Elem.Brand & ")";

END Causal.
