GENERIC MODULE WideIntOps(Type);
IMPORT DynamicInt, NativeInt;
IMPORT Mpz;
IMPORT Word;

PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T =
  BEGIN
    Mpz.and(t, x, Type.Mask);
    Mpz.RightShift(x, x, Type.Width);
    RETURN x
  END unpack_dynamic;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T =
  BEGIN
    Mpz.set_ui(t, x);
    RETURN Word.RightShift(x, Type.Width)
  END unpack_native;

PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T =
  BEGIN
    Mpz.LeftShift(x, x, Type.Width);
    Mpz.ior(x, x, t);
    RETURN x 
  END pack_dynamic;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T =

  VAR
    res : NativeInt.T;
  BEGIN
    (* not sure this makes sense *)
    res := Word.Shift(x, Type.Width);
    res := Word.Or(res, Mpz.ToWord(t));
    RETURN res
  END pack_native;

BEGIN END WideIntOps.
