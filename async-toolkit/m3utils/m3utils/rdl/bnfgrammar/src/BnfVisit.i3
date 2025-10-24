(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE BnfVisit;
IMPORT Bnf;

TYPE
  T = OBJECT METHODS visit(t : Bnf.T)  END;

PROCEDURE Pre(t : Bnf.T; visitor : T);

PROCEDURE Post(t : Bnf.T; visitor : T);

CONST Brand = "BnfVisit";

END BnfVisit.
