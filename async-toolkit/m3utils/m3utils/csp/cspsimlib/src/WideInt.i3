(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE WideInt;
IMPORT Word;

TYPE T = ARRAY OF Word.T; (* 2s complement *)

PROCEDURE Format(READONLY a : T) : TEXT;
  
END WideInt.
