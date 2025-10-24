(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE NodeRec;
IMPORT Nodes, Dims;

TYPE
  T = OBJECT
    nm  : TEXT;           (* full name, including indexing *)
    sNm : TEXT;           (* name w/o DUT prefix *)
    nds : Nodes.T;
    idx : REF Dims.T;
  END;

CONST Brand = "NodeRec";

END NodeRec.
