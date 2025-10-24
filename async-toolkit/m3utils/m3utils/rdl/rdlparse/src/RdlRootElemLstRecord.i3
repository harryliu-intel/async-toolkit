(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlRootElemLstRecord;
IMPORT RdlRootElemList;
IMPORT RdlPropertySymtab;
IMPORT RdlComponentDefSymtab;

TYPE
  T = RECORD
    lst     : RdlRootElemList.T;
    propTab : RdlPropertySymtab.T;
    defTab  : RdlComponentDefSymtab.T;
  END;

CONST Brand = "RdlRootElemLstRecord";

END RdlRootElemLstRecord.
