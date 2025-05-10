MODULE CspBoolean;
IMPORT NativeInt;
IMPORT DynamicInt;
IMPORT Word;
IMPORT Mpz;

PROCEDURE ToInteger(t : T) : INTEGER =
  BEGIN
    IF t THEN RETURN -1 ELSE RETURN 0 END
  END ToInteger;

PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T =
  BEGIN
    Mpz.and(scratch, x, MpzMask);
    t := Mpz.ToInteger(x) = 1;
    Mpz.RightShift(x, x, Width);
    RETURN x
  END unpack_dynamic;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T =
  BEGIN
    t := Word.And(x, Mask) = 1;
    x := Word.RightShift(x, Width);
    RETURN x
  END unpack_native;

CONST A = ARRAY T OF [0..1] { 0 , 1 };
      
PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T =
  BEGIN
    Mpz.set_ui(scratch, A[t]);
    Mpz.LeftShift(x, x, Width);
    Mpz.ior(x, x, scratch);
    RETURN x
  END pack_dynamic;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T =
  VAR
    res : NativeInt.T;
  BEGIN
    res := Word.Shift(x, Width);
    res := Word.Or(res, A[t]);
    RETURN res
  END pack_native;

VAR
  MpzMask := Mpz.NewWord(Mask);
BEGIN END CspBoolean.
