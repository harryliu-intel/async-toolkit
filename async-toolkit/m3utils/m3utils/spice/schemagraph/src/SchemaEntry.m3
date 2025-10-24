(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SchemaEntry;
IMPORT LongrealType;

PROCEDURE CompareByX(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.x, b.x)
  END CompareByX;

BEGIN END SchemaEntry.
