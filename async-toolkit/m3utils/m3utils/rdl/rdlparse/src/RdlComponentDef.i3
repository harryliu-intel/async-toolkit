(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlComponentDef;
IMPORT RdlRootElem;

(* must be opaque because of circular reference *)

TYPE T <: RdlRootElem.T;

CONST Brand = "RdlComponentDef";
      Equal : PROCEDURE (a, b : T): BOOLEAN = NIL;
      
END RdlComponentDef.
