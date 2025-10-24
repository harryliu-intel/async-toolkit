(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PolySegment16;
IMPORT Rep16;

TYPE
  T = RECORD
    r      : Rep16.T;
    lo     : INTEGER;
    n      : Rep16.Count;
  END;

CONST Brand = "PolySegment16";

PROCEDURE Format(READONLY t : T; full : BOOLEAN) : TEXT;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
  
END PolySegment16.
