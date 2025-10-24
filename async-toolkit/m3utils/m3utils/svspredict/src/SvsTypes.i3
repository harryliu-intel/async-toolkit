(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SvsTypes;

TYPE
  CornerData = RECORD
    vtiming, vpower, sigma : LONGREAL;
  END;

PROCEDURE FmtCornerData(READONLY cd : CornerData) : TEXT;

CONST Brand = "SvsTypes";

END SvsTypes.
