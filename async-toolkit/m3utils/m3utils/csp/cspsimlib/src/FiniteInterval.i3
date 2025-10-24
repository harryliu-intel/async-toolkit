(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FiniteInterval;
IMPORT BigInt;

TYPE (* a T runs from lo to hi inclusive: [ lo, hi ] *)
  T = RECORD
    lo, hi : BigInt.T;
  END;

PROCEDURE Construct(lo, hi : BigInt.T) : T;
  
CONST Brand = "FiniteInterval";

END FiniteInterval.
