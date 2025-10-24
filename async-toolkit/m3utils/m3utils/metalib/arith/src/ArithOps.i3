(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ArithOps;
IMPORT ArithR;

TYPE R = ArithR.T;

TYPE RealFunction = OBJECT METHODS f(x : LONGREAL) : LONGREAL; END;

PROCEDURE Plus(a, b : R) : R;

END ArithOps.

