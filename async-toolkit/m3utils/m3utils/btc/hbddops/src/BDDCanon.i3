(* $Id: BDDCanon.i3,v 1.1 2014/02/09 11:16:08 mika Exp $ *)

INTERFACE BDDCanon;
IMPORT BDD;

(* canonicalize a boolean function of any number of variables.

   very inefficient code at the moment, but should be OK for small
   #s of variables.. *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(invertInputs, invertOutput : BOOLEAN) : T;
    add(x : BDD.T) : Canonical;
    size() : CARDINAL;
  END;

  Canonical = REF RECORD
    canLit  : REF ARRAY OF BDD.T;   (* literals of canonical cell *)
    canon   : BDD.T;                (* expression in terms of canonical lits *)

    actLit  : REF ARRAY OF BDD.T;   (* literals of actual cell *)
    inVert  : REF ARRAY OF BOOLEAN; (* whether literals are inverted *)
    outVert : BOOLEAN;              (* whether output is inverted *)
  END;

CONST Brand = "BDDCanon";

END BDDCanon.
