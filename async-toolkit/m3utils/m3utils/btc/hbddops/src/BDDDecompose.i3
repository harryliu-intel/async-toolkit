(* $Id$ *)

INTERFACE BDDDecompose;
IMPORT BDDDepender, BDD;

(* 
   Shen-McKellar-Weiner algorithm for boolean decomposition.
   See Knuth V4A, sec 7.1.2 
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init   (dep : BDDDepender.T) : T;

    attempt(bx : BDD.T; pfx : TEXT) : Result;
    (* attempt to decompose bx, returning Result.
       internal nodes are named (via BDD.New) using the prefix pfx *)

    minterms(bx : BDD.T; pfx : TEXT) : Result;
    (* brute-force approach using minterm expansion *)
    
  END;

TYPE 
  Result = REF RECORD
    x    : BDD.T;
    v    : BDD.T;
    next : Result;
  END;
(* interpretation of Result:
   if v = next = NIL, then x is the root node of the calculation
   (it is the node that has the value of the given input).  x may or
   may not have new input nodes.

   if x has new input nodes, then they will be defined in prior records.

   if v, next # NIL, then it is a definition of the new internal node
   v, with the formula x.

   the root node is always the last record in the list *)

CONST Brand = "BDDDecompose";

PROCEDURE Minterms(t : T; x : BDD.T; pfx : TEXT) : Result;

END BDDDecompose.

