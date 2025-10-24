(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyGroup;
IMPORT LibertyComponent;
IMPORT LibertyHead;
IMPORT LibertyStatementSeq;

TYPE
  T <: Public;

  Public = LibertyComponent.T OBJECT
    head       : LibertyHead.T;
    statements : LibertyStatementSeq.T;
  END;

CONST Brand = "LibertyGroup";

END LibertyGroup.
 
