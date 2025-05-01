GENERIC INTERFACE NarrowIntOps(Type);
IMPORT Word;

CONST Brand = "NarrowIntOps(" & Type.Brand & ")";

PROCEDURE SignExtend(w : Word.T) : INTEGER;
  (* the bits of an instance of Type.T have been written into w.
     Sign-extend the bits of w to make a properly formed Modula-3 INTEGER *)

END NarrowIntOps.
