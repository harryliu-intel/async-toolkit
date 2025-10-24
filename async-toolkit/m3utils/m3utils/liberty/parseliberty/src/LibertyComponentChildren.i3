(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LibertyComponentChildren;
IMPORT LibertyComponent, LibertyComponentSeq;

REVEAL
  LibertyComponent.T <: Private;

TYPE
  Private = LibertyComponent.Public OBJECT METHODS
    canHaveChildren() : BOOLEAN;
    children() : LibertyComponentSeq.T;
    debugDump(truncate : CARDINAL := LAST(CARDINAL)) : TEXT;
  END;

CONST Brand = "LibertyComponentChildren";

END LibertyComponentChildren.
    
  
