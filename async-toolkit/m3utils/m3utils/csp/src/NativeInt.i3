INTERFACE NativeInt;
IMPORT Word;

(* these are native operations on INTEGERs *)
(* the system excludes Word.T with the MSB set, as they have different
   interpretation as INTEGER vs. Word.T (they are instead handled as WideInt) *)

CONST And = Word.And;
      Or  = Word.Or;
      Xor = Word.Xor;
      Shl = Word.Shift; (* right shift is done by negating the shift amount *)


PROCEDURE Pow(a, b : INTEGER) : INTEGER;

END NativeInt.
