(* $Id$ *)
GENERIC INTERFACE BrandedSet(Elem, Set);

TYPE T = Set.T;

CONST Brand = "Set of " & Elem.Brand;

END BrandedSet.
