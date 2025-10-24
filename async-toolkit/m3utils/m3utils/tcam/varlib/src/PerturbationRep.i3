(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PerturbationRep;
IMPORT Perturbation;
IMPORT TextLongRealTbl AS TextLRTbl;

REVEAL
  Perturbation.Default <: Private;
  
TYPE
  Private = Perturbation.PubDefault OBJECT
    model, var  : TEXT;
  END;
  
END PerturbationRep.
