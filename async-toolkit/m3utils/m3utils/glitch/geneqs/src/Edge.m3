(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Edge;
IMPORT Text;

CONST TE = Text.Equal;
      
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN TE(a.from, b.from) AND TE(a.to, b.to) END Equal;

BEGIN END Edge.
