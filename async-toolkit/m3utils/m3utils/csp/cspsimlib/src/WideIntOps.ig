(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE WideIntOps(Type);
IMPORT Word;
IMPORT DynamicInt, NativeInt;

TYPE T = Type.T;

PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T;

PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T;

PROCEDURE FromWordArray(VAR t : T; READONLY a : ARRAY OF Word.T);

PROCEDURE ToWordArray(READONLY t : T; VAR a : ARRAY OF Word.T);

END WideIntOps.
