(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Schema;
IMPORT FDataSeq;

TYPE
  T = RECORD 
    nfields : CARDINAL;           (* total # of fields *)
    dfields : CARDINAL;           (* expected # of data fields *)
    fdata   : FDataSeq.T;         (* specific info on a field *)
  END;

CONST Brand = "Schema";

END Schema.
