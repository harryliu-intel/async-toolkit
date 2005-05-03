(* $Id$ *)

GENERIC INTERFACE Cache(Key, Value);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(cacheSize : CARDINAL := 10) : T;

    get(idx : Key.T) : Value.T;
    (* get value *)
    
    compute(idx : Key.T) : Value.T;
    (* override this *)

    haveCachedData(idx : Key.T) : BOOLEAN;
    (* use for optimizations *)
  END;

CONST Brand = "Cache (" & Key.Brand & "," & Value.Brand & ")";

END Cache.
