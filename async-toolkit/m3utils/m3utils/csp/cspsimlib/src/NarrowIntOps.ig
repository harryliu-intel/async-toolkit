GENERIC INTERFACE NarrowIntOps(Type);
IMPORT Word;
IMPORT DynamicInt, NativeInt;

TYPE T = Type.T;

CONST Brand = "NarrowIntOps(" & Type.Brand & ")";

PROCEDURE SignExtend(w : Word.T) : INTEGER;
  (* the bits of an instance of Type.T have been written into w.
     Sign-extend the bits of w to make a properly formed Modula-3 INTEGER *)

PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T;

PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T;

PROCEDURE FromWordArray(VAR t : T; READONLY a : ARRAY OF Word.T);

PROCEDURE ToWordArray(READONLY t : T; VAR a : ARRAY OF Word.T);
  
END NarrowIntOps.
