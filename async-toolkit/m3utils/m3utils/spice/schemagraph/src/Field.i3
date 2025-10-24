(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Field;

TYPE T = { Ignore, Collate, Sweep, Report, Formula };

CONST Names = ARRAY T OF TEXT { "ignore", "collate", "sweep", "report", "formula" };

CONST Brand = "Field";

END Field.
