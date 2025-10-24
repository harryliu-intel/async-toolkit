(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlEnumDef;
IMPORT RdlRootElem;
IMPORT RdlEnumEntryList;

TYPE
  T = RdlRootElem.T BRANDED Brand OBJECT
    id : TEXT;
    body : RdlEnumEntryList.T;
  END;

CONST Brand = "RdlEnumDef";

END RdlEnumDef.
