(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DecoratedComponentDef;
IMPORT RdlComponentDef;
IMPORT RegComponent;

TYPE
  T <: Public;

  Public = RdlComponentDef.T OBJECT
    comp : RegComponent.T;
  METHODS
    init(old : RdlComponentDef.T; comp : RegComponent.T) : T;
  END;

CONST Brand = "DecoratedComponentDef";

END DecoratedComponentDef.
