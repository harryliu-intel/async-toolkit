(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Sim;

TYPE   T     =                 {  XA,   HSPICE };

CONST  Names = ARRAY T OF TEXT { "xa", "hspice" };

CONST Brand = "Sim";

END Sim.
