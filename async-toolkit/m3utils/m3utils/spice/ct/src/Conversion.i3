(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Conversion;
IMPORT Pathname;

TYPE
  T = RECORD
    ifn, ofn : Pathname.T;
  END;

CONST Brand = "Conversion";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;
      
END Conversion.
