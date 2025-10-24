(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC MODULE NarrowIntOps(Type);
IMPORT Word;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Unsigned;
IMPORT DynamicInt, NativeInt;
IMPORT Mpz;

CONST doDebug = FALSE;
      
PROCEDURE SignExtend(w : Word.T) : INTEGER =
  BEGIN
    IF doDebug THEN
      Debug.Out(Brand & ".SignExtend(16_" & Unsigned(w) & ")")
    END;
    IF Type.Signed THEN
      VAR
        sb := Word.Extract(w, Type.Width - 1, 1);
      BEGIN
        IF doDebug THEN
          Debug.Out("SignExtend(sb = " & Unsigned(sb) & ")")
        END;
        IF sb = 1 THEN
          WITH res = Word.Or(Type.NotMask, w) DO
            IF doDebug THEN
              Debug.Out(F("SignExtend(res = %s [16_%s]",
                          Int(res), Unsigned(res)))
            END;
            RETURN res
          END
        ELSE
          RETURN w
        END
      END
    ELSE
      RETURN w
    END
  END SignExtend;
  
PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T =
  BEGIN
    Mpz.and(scratch, x, MpzMask);
    t := Mpz.ToInteger(scratch);
    Mpz.RightShift(x, x, Type.Width);
    RETURN x
  END unpack_dynamic;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T =
  BEGIN
    t := Word.And(x, Type.Mask);
    x := Word.RightShift(x, Type.Width);
    RETURN x
  END unpack_native;

PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T =
  BEGIN
    Mpz.set_ui(scratch, t);
    Mpz.LeftShift(x, x, Type.Width);
    Mpz.ior(x, x, scratch);
    RETURN x
  END pack_dynamic;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T =
  VAR
    res : NativeInt.T;
  BEGIN
    res := Word.Shift(x, Type.Width);
    res := Word.Or(res, t);
    RETURN res
  END pack_native;

PROCEDURE FromWordArray(VAR t : T; READONLY a : ARRAY OF Word.T) =
  BEGIN
    t := Word.And(a[0], Type.Mask)
  END FromWordArray;

PROCEDURE ToWordArray(READONLY t : T; VAR a : ARRAY OF Word.T) =
  BEGIN
    a[0] := t;
    FOR i := 1 TO LAST(a) DO
      a[i] := 0
    END
  END ToWordArray;

VAR
  MpzMask := Mpz.NewWord(Type.Mask);
BEGIN END NarrowIntOps.
