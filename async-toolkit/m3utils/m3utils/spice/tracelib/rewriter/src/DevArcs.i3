(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DevArcs;
IMPORT Word;

TYPE
  T = RECORD
    pfx, sfx : TEXT;
  END;

CONST Brand = "DevNodes";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

END DevArcs.
