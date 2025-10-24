(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TvpMeasurement;


TYPE
  T = RECORD
    t, (* temp in degrees Celsius *)
    v, (* voltage in volts *)
    p  (* power in watts *)         : LONGREAL;
  END;

CONST Brand = "TvpMeasurement";

PROCEDURE Fmt(READONLY a : T) : TEXT;
  
END TvpMeasurement.
