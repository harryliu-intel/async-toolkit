(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LrArray;
IMPORT Word;

TYPE T = REF ARRAY OF LONGREAL;

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

CONST Brand = "LrArray";

END LrArray.
