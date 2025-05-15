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
      
END DynamicInt.
