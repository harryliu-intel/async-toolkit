(* $Id$ *)

INTERFACE AdGridQ;
IMPORT Word, AdGridChild AS Child;
IMPORT LRPoint;

(* "adaptive grid quadrilateral *)

TYPE 
  T = OBJECT METHODS
    corner(c : Child.T) : LRPoint.T;    

    getLbound(adGrid : REFANY) : LONGREAL;
    getUbound(adGrid : REFANY) : LONGREAL;

    subdivide(levels : CARDINAL := 1) : REFANY; (* returns AdGridQSet.T of new tiles *)
  END;

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "AdGridQ";

END AdGridQ.
