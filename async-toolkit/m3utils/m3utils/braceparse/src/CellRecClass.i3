(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CellRecClass;
IMPORT CellRec;
IMPORT Subcell;
IMPORT MosInfoCardTbl;

TYPE
  Private = CellRec.Public OBJECT
    subcells : REF ARRAY OF Subcell.T;
    mosTbl   : MosInfoCardTbl.T;
    aux      : CARDINAL; (* value slot for client computations *)
  END;

REVEAL
  CellRec.T <: Private;

END CellRecClass.
