(* $Id$ *)

GENERIC INTERFACE Fifo(Elem);

TYPE 
  T <: Public;

  Public = MUTEX OBJECT METHODS
    init() : T;

    put(t : Elem.T);

    get() : Elem.T;

    empty() : BOOLEAN;

    member(t : Elem.T) : BOOLEAN;

    size() : CARDINAL;
  END;

CONST Brand = "Fifo";

END Fifo.
