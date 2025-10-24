(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MpfrRoundingMode;

TYPE T = {
  N,    (* round to nearest, with ties to even        *)
  Z,    (* round toward zero                          *)
  U,    (* round toward +Inf                          *)
  D,    (* round toward -Inf                          *)
  A,    (* round away from zero                       *)
  F,    (* faithful rounding                          *)
  NA    (* round to nearest, with ties away from zero *)
  };
  
CONST Brand = "MpfrRoundingMode";

END MpfrRoundingMode.
