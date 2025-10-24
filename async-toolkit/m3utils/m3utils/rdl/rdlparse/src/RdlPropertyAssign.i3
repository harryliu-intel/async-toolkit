(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlPropertyAssign;
IMPORT RdlRootElem;

TYPE T = RdlRootElem.T BRANDED OBJECT END;

CONST Brand = "RdlPropertyAssign";

      Equal : PROCEDURE (a, b : T) : BOOLEAN = NIL;
      
END RdlPropertyAssign.
