(* $Id: Subarrays.ig,v 1.1 2006/03/19 00:10:06 mika Exp $ *)

GENERIC INTERFACE Subarrays(Elem);

(* a helpful interface for iterating through subarrays of a set in some order

   Given N objects of type Elem.T, generate all subarrays of M objects out
   of those N for M <= N.  The M objects will always be in the same order
   as the initial N objects.

   If the base array a is changed while the iterator is live, the new
   elements will be returned in place of the original elements.

   Do this as follows:

   a := NEW(REF ARRAY OF Elem.T, N);
   
   FOR i := FIRST(a^) TO LAST(a^) DO
     initialize(a[i])
   END;

   VAR
     iter := ElemSubarrays.IterateOfSize(a,M);
   BEGIN
     WHILE iter.next(b) DO
       use(b)
     END
   END
*)

TYPE RarrC = REF arrC;
      arrC =     ARRAY OF CARDINAL;

TYPE
  Iterator <: PublicIterator;

  PublicIterator = OBJECT METHODS
    next(VAR arr : ARRAY OF Elem.T) : BOOLEAN;
  END;

PROCEDURE IterateOfSize(arr : REF ARRAY OF Elem.T; size : CARDINAL) : Iterator;

CONST Brand = "Subarrays(" & Elem.Brand & ")";

END Subarrays.
