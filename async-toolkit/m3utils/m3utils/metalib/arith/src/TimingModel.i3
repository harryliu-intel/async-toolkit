(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TimingModel;
IMPORT TransitionList, Name, ArithP;

TYPE
  T = OBJECT METHODS
    transitionTime(of       : Name.T;
                   newValue : BOOLEAN;
                   fanins   : TransitionList.T;
                   minMult  : LONGREAL) : ArithP.T;
  END;

END TimingModel.
