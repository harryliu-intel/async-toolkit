(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlExplicitComponentInst;
IMPORT RdlOrigin, RdlComponentInstElemList;
IMPORT RdlRootElem;

TYPE
  T = RdlRootElem.T OBJECT
    haveOrigin : BOOLEAN;
    origin     : RdlOrigin.T;
    alias      : TEXT;
    id         : TEXT;
    list       : RdlComponentInstElemList.T;
  END;

CONST Brand = "RdlExplicitComponentInst";

END RdlExplicitComponentInst.
