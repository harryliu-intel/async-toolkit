(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MultiEvalClass;
IMPORT MultiEval, LRScalarField;

REVEAL
  MultiEval.T <: Private;

TYPE
  Private = MultiEval.Public OBJECT
    base : LRScalarField.T;
  END;

END MultiEvalClass.
