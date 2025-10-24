(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspStatementClass;

TYPE
  Assignment = T OBJECT
    lhs, rhs : CspExpression.T;
  END;
  
END CspStatementClass.
