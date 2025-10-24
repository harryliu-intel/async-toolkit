(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlPostPropertyAssign;
IMPORT RdlInstanceRef;
IMPORT RdlPropertyAssignRhs;

TYPE
  T = OBJECT
    instanceRef : RdlInstanceRef.T;
    rhs         : RdlPropertyAssignRhs.T;
  END;

CONST Brand = "RdlPostPropertyAssign";
      
END RdlPostPropertyAssign.
