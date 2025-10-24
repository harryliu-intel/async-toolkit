(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Simulation;
IMPORT Pathname, ProcUtils;

TYPE
  T = RECORD
    cmd  : TEXT;
    path : Pathname.T;
    cm   : ProcUtils.Completion;
  END;

CONST Brand = "Simulation";

END Simulation.
