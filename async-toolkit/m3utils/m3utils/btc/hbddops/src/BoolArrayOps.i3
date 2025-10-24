(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: BoolArrayOps.i3,v 1.1 2014/02/09 11:16:08 mika Exp $ *)

INTERFACE BoolArrayOps;

PROCEDURE ColsEq(READONLY a : ARRAY OF ARRAY OF BOOLEAN;
                 p, q : CARDINAL) : BOOLEAN;

PROCEDURE MaxTwoDiffCols(READONLY a : ARRAY OF ARRAY OF BOOLEAN) : BOOLEAN;

END BoolArrayOps.
