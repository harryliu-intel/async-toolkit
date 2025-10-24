(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ArithOps;
IMPORT ArithRep;

PROCEDURE Plus(a, b : R) : R =
  BEGIN RETURN NEW(ArithRep.RPlus, a := a, b := b) END Plus;

BEGIN END ArithOps.
