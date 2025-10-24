(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlAnonComponentInstElems;
IMPORT RdlComponentInstElemList;

TYPE
  T = OBJECT
    external : BOOLEAN;
    list : RdlComponentInstElemList.T;
  END;

CONST Brand = "RdlComponentInstElems";

END RdlAnonComponentInstElems.
