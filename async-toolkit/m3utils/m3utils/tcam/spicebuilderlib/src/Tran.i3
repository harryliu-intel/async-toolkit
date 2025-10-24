(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Tran;

TYPE 
  T = RECORD
    t,     (* start of transition  *)
    v,     (* target of transition *)
    rf     (* rise/fall time       *)
    : LONGREAL;
  END;

CONST Brand = "Tran";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END Tran.
    
