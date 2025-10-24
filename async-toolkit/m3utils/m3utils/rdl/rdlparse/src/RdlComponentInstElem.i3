(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RdlComponentInstElem;
IMPORT RdlArray, RdlNum;

TYPE
  T = OBJECT
    id : TEXT;
    array : RdlArray.T;
    eq, at, inc, mod : RdlNum.T;
  END;

CONST Brand = "RdlComponentInstElem";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;
      
END RdlComponentInstElem.
