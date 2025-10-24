(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE BnfType;
IMPORT Bnf;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN RETURN Bnf.Equal(a, b) END Equal;

BEGIN END BnfType.
