(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BDDSetHash;
IMPORT BDDSetDef, BDDSet;
IMPORT Word;

TYPE T <: BDDSetDef.T;

PROCEDURE Hash(a : T) : Word.T;

CONST Equal = BDDSet.Equal;

CONST Brand = "BDDSetHash";

END BDDSetHash.
