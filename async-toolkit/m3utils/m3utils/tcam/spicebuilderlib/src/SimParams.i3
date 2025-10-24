(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SimParams;

TYPE
  T = RECORD
    step    : LONGREAL;
    maxTime : LONGREAL; (* overridden by length of sequence *)
  END;

END SimParams.
