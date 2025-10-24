(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Scale;

PROCEDURE LkgPwrByT(from, to : LONGREAL) : LONGREAL;

PROCEDURE LkgPwrByV(from, to : LONGREAL) : LONGREAL;

PROCEDURE DynPwrByT(<*UNUSED*>from, to : LONGREAL) : LONGREAL;

PROCEDURE DynPwrByV(from, to : LONGREAL) : LONGREAL;

END Scale.
