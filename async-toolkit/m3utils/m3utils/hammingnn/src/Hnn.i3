INTERFACE Hnn;

(* 
   K-nearest neighbors in the Hamming space

   Author : Mika Nystroem <mika.nystroem@intel.com>
   October, 2022
*)

TYPE
  Elem = ARRAY OF BOOLEAN;

  T <: Public;

  Public = OBJECT METHODS

    init(len : CARDINAL) : T;
    (* initialize a T holding elements of len bits *)

    put(READONLY elem : Elem) : CARDINAL;
    (* it is a checked runtime error to put a T that is not len bits long;
       returns its index *)
    
    iterClose(READONLY elem : Elem; maxHamming : CARDINAL) : Iterator;
    (* iterate over all stored strings that are no more than maxHamming
       from the search string *)
    
    iterNnOrdered(READONLY elem : Elem;
                  n : CARDINAL;
                  maxHamming : CARDINAL := 0) : Iterator;
    (* iterate over nn stored strings that are closest in Hamming distance,
       in nondecreasing order of distance *)

    iter() : Iterator;
    (* iterate over all members *)

    get(i : CARDINAL; VAR elem : Elem);
    (* get the ith member *)

    size() : CARDINAL;
    (* count of members *)

    getLen() : CARDINAL;
    (* length of members in bits *)
  END;

  Iterator = OBJECT METHODS
    next(VAR e : Elem) : BOOLEAN;
  END;
  
CONST Brand = "Hnn";

END Hnn.
