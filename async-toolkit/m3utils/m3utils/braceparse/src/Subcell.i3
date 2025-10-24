(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Subcell;
IMPORT CellRec;
IMPORT InstanceName;

TYPE
  T = RECORD
    type     : CellRec.T;

    instance : InstanceName.T;
    (* representation: 
       if   ARRAY[7] = 0 then a C-style string
       else a little-endian byte index into an external list of names,
       zero-separated
    *)
  END;

CONST Brand = "Subcell";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;

END Subcell.
