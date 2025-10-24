(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MemoTranSeq;
IMPORT TranSeq;

TYPE T <: TranSeq.T;

PROCEDURE Interpolate(s : T; tm : LONGREAL) : LONGREAL;

END MemoTranSeq.
