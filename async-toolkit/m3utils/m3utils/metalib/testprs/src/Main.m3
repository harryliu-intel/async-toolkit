(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Example, Params;

BEGIN
  EVAL Example.DoIt(Params.Get(1))
END Main.
