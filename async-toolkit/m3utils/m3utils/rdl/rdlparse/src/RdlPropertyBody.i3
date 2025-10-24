(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlPropertyBody;
IMPORT RdlPropertyType, RdlPropertyComponentDisjunction, RdlPropertyDefault;

TYPE
  T = BRANDED Brand OBJECT
    type  : RdlPropertyType.T;
    usage : RdlPropertyComponentDisjunction.T;
    def   : RdlPropertyDefault.T;
  END;

CONST Brand = "RdlPropertyBody";

END RdlPropertyBody.
