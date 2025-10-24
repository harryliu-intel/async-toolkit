(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlEnumEntry;
IMPORT RdlNum;
IMPORT RdlEnumPropertyAssignList;

TYPE
  T = OBJECT
    id : TEXT;
    num : RdlNum.T;
    properties : RdlEnumPropertyAssignList.T;
  END;

CONST Brand = "RdlEnumEntry";

      Equal : PROCEDURE (a, b : T): BOOLEAN = NIL;

END RdlEnumEntry.
