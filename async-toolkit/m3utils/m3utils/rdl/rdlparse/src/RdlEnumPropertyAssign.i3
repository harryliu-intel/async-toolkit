(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlEnumPropertyAssign;

TYPE T = ROOT BRANDED OBJECT END;

     Name = T BRANDED OBJECT str : TEXT END;

     Desc = T BRANDED OBJECT str : TEXT END;

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

      Brand = "RdlPropertyAssign";

END RdlEnumPropertyAssign.
