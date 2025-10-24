(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TextTable;
IMPORT TextTextTbl;
IMPORT HTMLTable;

TYPE 
  T <: Public;

  Public = TextTextTbl.Default OBJECT
  METHODS
    toHTML() : HTMLTable.T;
  END;

END TextTable.
