(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Transition;
IMPORT Refany, Word;

TYPE  T <: ROOT;

CONST Brand = "Transition";
      
CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;

CONST Compare : PROCEDURE(a, b : T) : [-1..1] = NIL;

END Transition.
