(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CspExpressionPublic;
IMPORT CspExpression;
IMPORT CspExpressionSeq;
IMPORT Atom;
IMPORT CspRange;

REVEAL
  CspExpression.FunctionCall <: PublicFunctionCall;
  CspExpression.Loop         <: PublicLoop;
  
TYPE
  PublicFunctionCall = CspExpression.T OBJECT
    f    : CspExpression.T;
    args : CspExpressionSeq.T;
  END;

  PublicLoop = CspExpression.T OBJECT
    dummy : Atom.T;
    range : CspRange.T;
    op    : CspExpression.BinaryOp;
    x     : CspExpression.T;
  END;
  
END CspExpressionPublic.
