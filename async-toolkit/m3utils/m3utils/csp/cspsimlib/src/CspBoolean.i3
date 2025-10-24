(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspBoolean;
IMPORT NativeInt;
IMPORT DynamicInt;
IMPORT Word;

TYPE T = BOOLEAN;

CONST Brand = "CspBoolean";

PROCEDURE ToInteger(t : T) : INTEGER;
  (* return -1 for TRUE, 0 for FALSE *)

CONST Width = 1;

PROCEDURE unpack_dynamic(VAR t : T; x, scratch : DynamicInt.T) : DynamicInt.T;

PROCEDURE unpack_native(VAR t : T; x : NativeInt.T) : NativeInt.T;

PROCEDURE pack_dynamic(x, scratch : DynamicInt.T; READONLY t : T) : DynamicInt.T;

PROCEDURE pack_native(x : NativeInt.T; READONLY t : T) : NativeInt.T;

CONST Mask    = 16_1;
      NotMask = Word.Not(Mask);
      Wide    = FALSE;
      Signed  = FALSE;
      
END CspBoolean.
  
