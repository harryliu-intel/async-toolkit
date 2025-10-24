(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PolySegment;
IMPORT LRRegression AS Regression;

TYPE
  T = RECORD
    r  : Regression.T;
    lo : INTEGER;
    n  : CARDINAL;
  END;

CONST Brand = "PolySegment";

END PolySegment.
