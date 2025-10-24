(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE OptCallback;

TYPE
  T = OBJECT METHODS
    command(samples : [-1..LAST(CARDINAL) ] := 0) : TEXT;
  END;
  (* 
     samples=0 means a single sample  
     
     samples=-1 means a single nominal evaluation
  *)

CONST Brand = "OptCallback";

END OptCallback.
