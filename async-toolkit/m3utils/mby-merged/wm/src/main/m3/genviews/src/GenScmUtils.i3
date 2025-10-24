(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE GenScmUtils;
IMPORT TextSeq, RdlArray, RegCGenState;

PROCEDURE FmtConstant(sDecls : TextSeq.T; val : TEXT; nm, sfx : TEXT);

PROCEDURE FmtArrSz(sDecls : TextSeq.T; a : RdlArray.Single; nm : TEXT);

PROCEDURE PutSDecls(gs : RegCGenState.T; sDecls : TextSeq.T);

END GenScmUtils.
