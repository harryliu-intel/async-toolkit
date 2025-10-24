(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlPropertyDef;
IMPORT RdlPropertyBody;
IMPORT RdlRootElem;

TYPE
  T = RdlRootElem.T OBJECT
    id   : TEXT;
    body : RdlPropertyBody.T;
  END;

CONST Brand = "RdlPropertyDef";

END RdlPropertyDef.
