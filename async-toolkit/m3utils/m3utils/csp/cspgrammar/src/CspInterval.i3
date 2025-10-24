(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspInterval;
IMPORT BigInt;

TYPE
  T = RECORD
    left, right : BigInt.T;
  END;

CONST Brand = "CspInterval";

END CspInterval.
