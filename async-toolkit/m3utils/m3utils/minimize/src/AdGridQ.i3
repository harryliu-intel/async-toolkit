(* $Id$ *)

INTERFACE AdGridQ;
IMPORT Word, AdGridChild AS Child;
IMPORT LRPoint;

(* "adaptive grid quadrilateral *)

TYPE 
  T = OBJECT METHODS
    corner(c : Child.T) : LRPoint.T;    

    (* in the following, adGrid is an AdGrid.T *)
    getLbound(adGrid : REFANY) : LONGREAL;
    getUbound(adGrid : REFANY) : LONGREAL;

    getCornerValues(adGrid : REFANY) : ARRAY Child.T OF LONGREAL;

    subdivide(levels : CARDINAL := 1) : REFANY; (* returns AdGridQSet.T 
                                                   of new tiles *)

    (* find the neighbor tile in the specified direction.
       Three possibilities: 1. neighbor tile of same size
                            2. neighbor tile of larger size
                            3. NIL *)
    neighbor(at : LRPoint.T; dir : Dir) : T;

    getCrossings(adGridP : REFANY; of : LONGREAL): ARRAY Dir OF REF LRPoint.T;
  END;

  Dir = { N, E, W, S };

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "AdGridQ";

END AdGridQ.
