(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ArithProbability;
IMPORT ArithBits AS Bits;

TYPE
  T = RECORD
    lo, hi, count : Bits.Freq;
  END;

CONST Brand = "ArithProbability";

PROCEDURE Format(t : T) : TEXT;
      
END ArithProbability.
    
