INTERFACE ConstrainedSpace;
IMPORT LRMatrix2 AS Mat;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(radius : LONGREAL; READONLY p : Mat.V) : T;

    cons2cart(READONLY p : Mat.V; VAR e : Mat.V);
    (* map a N-1 p in constrained space to N in cartesian space *) 

  END;

END ConstrainedSpace.
