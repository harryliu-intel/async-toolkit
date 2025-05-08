MODULE DynamicInt;
IMPORT Word;
IMPORT Mpz;

PROCEDURE ConvertNativeInt(scratch : T; native : INTEGER) : T =
  BEGIN
    Mpz.set_si(scratch, native);
    RETURN scratch
  END ConvertNativeInt;
  
PROCEDURE ConvertNativeWord(scratch : T; native : Word.T) : T =
  BEGIN
    Mpz.set_ui(scratch, native);
    RETURN scratch
  END ConvertNativeWord;
 
BEGIN END DynamicInt.



