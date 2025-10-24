(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE WLRVector;
IMPORT LRVector;

TYPE
  T = RECORD
    v : LRVector.T;
    y : LONGREAL;
    w : LONGREAL;
  END;

CONST Brand = "WLRVector";

PROCEDURE Format(READONLY a : T) : TEXT;
      
END WLRVector.
