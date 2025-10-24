(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NetContext;

TYPE
  T = RECORD
    rem : CARDINAL; (* bytes remaining *)
  END;

CONST Brand = "NetContext";

EXCEPTION Short;
          
END NetContext.
