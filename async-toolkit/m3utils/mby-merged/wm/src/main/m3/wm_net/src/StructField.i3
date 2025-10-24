(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StructField;
IMPORT Word;

PROCEDURE Format(val : Word.T; wid : [1..BITSIZE(Word.T)]) : TEXT;

CONST Brand = "StructField";

END StructField.
