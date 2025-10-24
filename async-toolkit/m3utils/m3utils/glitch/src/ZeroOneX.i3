(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ZeroOneX;

TYPE T = { V0, V1, VX };
CONST Names = ARRAY T OF TEXT { "0", "1", "X" };

PROCEDURE FromBool(bool : BOOLEAN) : T;

PROCEDURE Format(t : T) : TEXT;

CONST Brand = "ZeroOneX";

END ZeroOneX.
