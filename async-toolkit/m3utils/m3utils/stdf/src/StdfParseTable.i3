(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfParseTable;
IMPORT StdfRecordTypes;

PROCEDURE Get(recTyp, recSub : CARDINAL; VAR res : StdfRecordTypes.T) : BOOLEAN;

CONST Brand = "StdfParseTable";

END StdfParseTable.
