(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE OscillatorTemp;
IMPORT Spline;
IMPORT DataPoint;

TYPE
  T = OBJECT
    temp       : LONGREAL;
    data       : REF ARRAY OF DataPoint.T; (* sorted by V *)
    tempcurve  : Spline.T;
    minV, maxV : LONGREAL;
  END;

CONST Brand = "OscillatorTemp";

END OscillatorTemp.
