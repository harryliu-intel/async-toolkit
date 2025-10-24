(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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

PROCEDURE FromWordArray(VAR t : T; READONLY a : ARRAY OF Word.T) =
  BEGIN
    Mpz.Import(t, a);
  END FromWordArray;

PROCEDURE ToWordArray(READONLY t : T; VAR a : ARRAY OF Word.T) =
  BEGIN
    FOR i := 0 TO LAST(a) DO
      a[i] := 0
    END;
    Mpz.Export(a, t)
  END ToWordArray;

PROCEDURE Remainder (f0 : T; f1 : T; f2 : T) =
  BEGIN
    IF Mpz.cmp(f2, Zero) = 0 THEN
      Mpz.set(f0, f1)
    ELSE
      Mpz.tdiv_r(f0, f1, f2)
    END
  END Remainder;
  
PROCEDURE Quotient (f0 : T; f1 : T; f2 : T) =
  BEGIN
    IF Mpz.cmp(f2, Zero) = 0 THEN
      Mpz.set(f0, Zero)
    ELSE
      Mpz.tdiv_q(f0, f1, f2)
    END
  END Quotient;

VAR
  Zero := Mpz.NewInt(0);
BEGIN
END DynamicInt.



