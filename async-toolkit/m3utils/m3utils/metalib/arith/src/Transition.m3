(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Transition;
IMPORT PRS, Boolean, Name, Word;

PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN Word.Plus(Name.Hash(a.name),Boolean.Hash(a.newValue)) END Hash;

BEGIN END Transition.
