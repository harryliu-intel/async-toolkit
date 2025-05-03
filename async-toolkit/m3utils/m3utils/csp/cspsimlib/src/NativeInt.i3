INTERFACE NativeInt;
IMPORT Word;
IMPORT Fmt AS FmtIntf;
IMPORT Mpz;

(* these are native operations on INTEGERs *)
(* the system excludes Word.T with the MSB set, as they have different
   interpretation as INTEGER vs. Word.T (they are instead handled as wide) *)

CONST And = Word.And;
      Or  = Word.Or;
      Xor = Word.Xor;
      Shl = Word.Shift; (* right shift is done by negating the shift amount *)


PROCEDURE Pow(a, b : T) : T;

CONST Format = FmtIntf.Int;

TYPE T = INTEGER;

CONST One  = 1;
      Zero = 0;

PROCEDURE Hex(a : T) : TEXT;

CONST ConvertDynamicInt = Mpz.ToInteger;
  
END NativeInt.
