(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TextCard;

TYPE
  T = RECORD
    t : TEXT;
    c : CARDINAL;
  END;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Compare(READONLY a, b : T) : [-1..1]; (* compare by c, then t *)

CONST Brand = "TextCard";

END TextCard.
