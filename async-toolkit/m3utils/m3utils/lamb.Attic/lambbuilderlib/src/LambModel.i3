(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE LambModel;
IMPORT Lamb;
IMPORT SimModel;

TYPE
  T <: Public;

  Public = SimModel.T OBJECT METHODS
    init(READONLY c : Lamb.T;
         cycleTime, assertHoldFrac, assertHoldTime : LONGREAL) : T;

  END;

CONST Brand = "LambModel";

END LambModel.
    
