(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE UpdateTracing;
IMPORT Env;

BEGIN
  Enabled := Env.Get("UPDATE_TRACING") # NIL;
END UpdateTracing.
