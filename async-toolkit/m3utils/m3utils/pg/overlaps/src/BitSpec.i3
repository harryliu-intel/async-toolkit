(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BitSpec;

TYPE
  T = { B0, B1, BQ };

CONST
  N = ARRAY T OF CHAR { '0', '1', '?' };

CONST Brand = "BitSpec";

END BitSpec.

