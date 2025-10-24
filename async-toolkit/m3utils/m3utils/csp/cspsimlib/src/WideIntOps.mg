(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC MODULE WideIntOps(Type);
IMPORT DynamicInt, NativeInt;
IMPORT Mpz;
IMPORT Word;

PROCEDURE unpack_dynamic(VAR t : T;
                         x : DynamicInt.T;
                         <*UNUSED*>scratch : DynamicInt.T) : DynamicInt.T =
  BEGIN
    Mpz.and(t, x, Type.Mask);
    Mpz.RightShift(x, x, Type.Width);
    RETURN x
  END unpack_dynamic;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T =
  BEGIN
    Mpz.set_ui(t, x);
    RETURN Word.Shift(x, -Type.Width)
  END unpack_native;

PROCEDURE pack_dynamic(x : DynamicInt.T;
                       <*UNUSED*>scratch : DynamicInt.T;
                       READONLY t : T) : DynamicInt.T =
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

PROCEDURE FromWordArray(VAR t : T; READONLY a : ARRAY OF Word.T) =
  BEGIN
    Mpz.Import(t, a);
    Type.ForceRange(t, t)
  END FromWordArray;

PROCEDURE ToWordArray(READONLY t : T; VAR a : ARRAY OF Word.T) =
  BEGIN
    FOR i := 0 TO LAST(a) DO
      a[i] := 0
    END;
    Mpz.Export(a, t)
  END ToWordArray;

BEGIN END WideIntOps.
