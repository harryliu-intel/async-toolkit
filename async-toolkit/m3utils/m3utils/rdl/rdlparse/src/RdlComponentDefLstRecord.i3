(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlComponentDefLstRecord;
IMPORT RdlComponentDefElemList;
IMPORT RdlPropertySymtab;
IMPORT RdlComponentDefSymtab;

TYPE
  T = RECORD
    lst     : RdlComponentDefElemList.T;
    propTab : RdlPropertySymtab.T;
    defTab  : RdlComponentDefSymtab.T;
    anonCnt : CARDINAL;
  END;

CONST Brand = "RdlComponentDefLstRecord";

END RdlComponentDefLstRecord.
