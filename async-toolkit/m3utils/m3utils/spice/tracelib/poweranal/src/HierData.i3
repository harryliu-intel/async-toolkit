(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE HierData;

TYPE
  T = RECORD
    instName : TEXT;
    typeName : TEXT;    (* type name of cell *)
    totalize : BOOLEAN; (* totalize or not when calc'ing energy *)
  END;

CONST Brand = "HierData";

END HierData.
