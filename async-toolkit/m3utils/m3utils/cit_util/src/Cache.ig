(* $Id$ *)

GENERIC INTERFACE Cache(Key, Value);

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(cacheSize : CARDINAL := 10) : T;
    (* cacheSize is ignored if adaptive caching is enabled, see below *)

    get(idx : Key.T) : Value.T;
    (* get value *)
    
    compute(idx : Key.T; staleValue : Value.T) : Value.T;
    (* override this---staleValue is an old, dirty value that has
       been evicted from the cache.  Can be used for object recycling
       as long as references from get aren't reused by client.
       (or ignored) *)

    haveCachedData(idx : Key.T) : BOOLEAN;
    (* use for optimizations *)

    purge();
    (* empty cache *)

  END;

CONST Brand = "Cache (" & Key.Brand & "," & Value.Brand & ")";

(* adaptive caching is false by def. *)
PROCEDURE EnableAdaptiveCaching(maxSize : CARDINAL; 
                                targetHitRate := 0.95d0;
                                startRatio := 0.33d0);
  (* enable adaptive cache size (adaptive caching).
     maxSize is the max size the cache is permitted to grow to.
     targetHitRate is the hit rate we seek.
     startRatio is the proportion of the maxSize we start with. *)

PROCEDURE DisableAdaptiveCaching();
  (* disable adaptive cache size *)

END Cache.
