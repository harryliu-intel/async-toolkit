(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyComponentSeqBuilder;
IMPORT LibertyComponentSeq;

TYPE T = LibertyComponentSeq.T;

PROCEDURE BuildSeq(c0, c1, c2, c3, c4, c5 : REFANY:=NIL) : T;

CONST Brand = "LibertyComponentSeqBuilder";

END LibertyComponentSeqBuilder.
