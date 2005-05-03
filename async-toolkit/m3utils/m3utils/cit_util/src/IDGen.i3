INTERFACE IDGen;


TYPE
  T = OBJECT METHODS
    alloc(): ID;
    free(id: ID);
  END;

  ID = CARDINAL;

    
(* "Low"

   Use a "Low" to allocate the lowest non-negative ID
   that is not *currently* in use.

   Memory consumption is at most proportional to the number of IDs currently
   in use, and delay is often sublinear (at least as good as "Region.T").
*)

  Low <: LowPublic;
  LowPublic = T OBJECT
  METHODS
    init(): Low;
  END;

  

(* "Unique"

   A "unique" never returns the same ID twice.

*)

  Unique <: UniquePublic;
  UniquePublic = T OBJECT
  METHODS
    init(): Unique;
  END;


END IDGen.
