(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CheckDir;
IMPORT Transition;
IMPORT Word;

TYPE T = SET OF Transition.Dir;

CONST All  = T { FIRST(Transition.Dir) .. LAST(Transition.Dir) };
      None = T {};

CONST Brand = "CheckDir";

PROCEDURE Fmt(t : T) : TEXT;
CONST Format = Fmt;
      
PROCEDURE Hash(t : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;
  
END CheckDir.
