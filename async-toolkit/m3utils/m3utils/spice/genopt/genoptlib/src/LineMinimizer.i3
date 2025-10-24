(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LineMinimizer;
IMPORT LRVector;
IMPORT LRScalarField;
IMPORT LineProblem;
IMPORT Thread;

CONST Brand = "LineMinimizer";

TYPE
  T <: Public;

  Public = Thread.Closure OBJECT METHODS
    init() : T;
    start(pp   : LRVector.T;
          dir  : LRVector.T;
          func : LRScalarField.T;
          rho  : LONGREAL);
    isDone() : BOOLEAN;
    wait() : LineProblem.T;
    quit(); (* will not interrupt running task, can be waited for *)
  END;

PROCEDURE Running() : CARDINAL;
  
END LineMinimizer.
