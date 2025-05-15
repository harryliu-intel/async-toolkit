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

PROCEDURE ConvertWideInt(scratch : T; wide : T) : T =
  BEGIN
    Mpz.set(scratch, wide);
    RETURN scratch
  END ConvertWideInt;

BEGIN END DynamicInt.



