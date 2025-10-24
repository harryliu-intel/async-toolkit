(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ParseProcRec;
IMPORT ParseProc;

TYPE
  T = RECORD
    nm : TEXT;              (* name as TEXT *)
    f  : ParseProc.T;
    ca : REF ARRAY OF CHAR; (* name as CHAR array -- for END matching *)
  END;    

CONST Brand = "ParseProcRec";

CONST Default = T { "**TOP**", NIL, NIL };

END ParseProcRec.
