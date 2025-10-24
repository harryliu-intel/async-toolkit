(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE OpenCharArray;
IMPORT Word;

TYPE T = ARRAY OF CHAR;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "OpenCharArray";

PROCEDURE Clone(READONLY a : T) : REF T;

END OpenCharArray.
