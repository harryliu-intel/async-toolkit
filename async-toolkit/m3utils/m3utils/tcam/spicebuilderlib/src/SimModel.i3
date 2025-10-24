(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SimModel;
IMPORT Valenv;

TYPE
  T = OBJECT METHODS
    simStep(e : Valenv.T);
  END;

CONST Brand = "SimModel";

END SimModel.
