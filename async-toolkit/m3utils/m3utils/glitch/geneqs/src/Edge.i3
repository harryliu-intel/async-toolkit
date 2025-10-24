(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Edge;

TYPE
  T = RECORD
    from, to : TEXT;
  END;

CONST Brand = "Edge";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Edge.
