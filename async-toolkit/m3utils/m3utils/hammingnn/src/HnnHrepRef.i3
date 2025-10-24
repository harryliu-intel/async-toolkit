(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HnnHrepRef;
IMPORT HnnHrep;
IMPORT Word;

TYPE T = REF HnnHrep.T;

CONST Brand = "Ref(" & HnnHrep.Brand & ")";

PROCEDURE Equal(a, b : T) : BOOLEAN;
  (* check whether a^.bits^ = b^.bits^ -- note not the same as HnnHrep *)

PROCEDURE Hash(a : T) : Word.T;

END HnnHrepRef.
     
