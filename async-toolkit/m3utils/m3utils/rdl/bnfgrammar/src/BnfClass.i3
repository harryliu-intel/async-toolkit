(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BnfClass;
IMPORT Bnf;

REVEAL
  Bnf.T <: Private;
  
TYPE
  Private = Bnf.Public OBJECT METHODS
    copy() : Bnf.T;
    deepCopy() : Bnf.T;
  END;

CONST Brand = "BnfClass";

END BnfClass.
  
