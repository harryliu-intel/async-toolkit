(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PRSClass;
IMPORT PRS, TimingModel;

TYPE 
  Private = PRS.Public OBJECT
    tm : TimingModel.T;
  END;

REVEAL PRS.T <: Private;

END PRSClass.
  
