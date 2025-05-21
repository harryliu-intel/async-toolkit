INTERFACE DynamicInt;
IMPORT Mpz;
IMPORT Word;

TYPE T = Mpz.T;

CONST Brand = "DynamicInt";

CONST Format = Mpz.Format;
CONST FormatHexadecimal = Mpz.FormatHexadecimal;

(* the following two routines convert the value into the scratch space
   and return it *)
PROCEDURE ConvertNativeInt(scratch : T; native : INTEGER) : T;
PROCEDURE ConvertNativeWord(scratch : T; native : Word.T) : T;

PROCEDURE ConvertWideInt(scratch : T; wide : T) : T;

CONST ConvertDynamicInt = ConvertWideInt; (* dumb *)

PROCEDURE FromWordArray(VAR t : T; READONLY a : ARRAY OF Word.T);

PROCEDURE ToWordArray(READONLY t : T; VAR a : ARRAY OF Word.T);

(* CSP style division defines that x/0 is zero and x%0 is x, implement blo: *)
PROCEDURE Remainder (f0 : T; f1 : T; f2 : T);
PROCEDURE Quotient (f0 : T; f1 : T; f2 : T);

END DynamicInt.
