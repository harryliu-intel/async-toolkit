(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspGuardedCommand;
IMPORT CspExpression;
IMPORT CspStatement;

TYPE
  T = OBJECT
    guard   : CspExpression.T;
    command : CspStatement.T;
  END;

CONST Brand = "CspGuardedCommand";

END CspGuardedCommand.
  
