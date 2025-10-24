(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MarginMeasurement;
IMPORT MarginScenario;

TYPE
  T = RECORD
    scenario : MarginScenario.T;
    margin   : LONGREAL;
    at       : LONGREAL;
  END;

PROCEDURE Compare(READONLY a, b : T) : [-1 .. 1];
  (* compare by margin *)

CONST Brand = "MarginMeasurement";

PROCEDURE Format(t : T) : TEXT;
  
END MarginMeasurement.
