(* $Id: SopRepG.ig,v 1.1 2014/01/15 04:38:30 mika Exp $ *)
GENERIC INTERFACE SopRepG(Sop, SopLiteral);

TYPE
  Conjunct = REF ARRAY OF SopLiteral.T;
  Rep = REF ARRAY OF Conjunct;

REVEAL Sop.T <: Private;

(* importers outside the "sop" package should treat the rep field as
   read-only. *)

TYPE Private = Sop.Public OBJECT rep : Rep; END;

END SopRepG.
