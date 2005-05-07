(* $Id$ *)

GENERIC INTERFACE Cache(Key, Value);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(cacheSize : CARDINAL := 10) : T;

    get(idx : Key.T) : Value.T;
    (* get value *)
    
    compute(idx : Key.T; staleValue : Value.T) : Value.T;
    (* override this---staleValue is an old, dirty value that has
       been evicted from the cache.  Can be used for object recycling
       as long as references from get aren't reused by client.
       (or ignored) *)

    haveCachedData(idx : Key.T) : BOOLEAN;
    (* use for optimizations *)
  END;

CONST Brand = "Cache (" & Key.Brand & "," & Value.Brand & ")";

(* adaptive caching is false by def. *)
PROCEDURE EnableAdaptiveCaching(maxSize : CARDINAL; targetHitRate := 0.95d0);
PROCEDURE DisableAdaptiveCaching();

END Cache.
