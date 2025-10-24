(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DataPoint;
IMPORT LongrealType;

PROCEDURE CompareVoltage(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.V, b.V)
  END CompareVoltage;
  
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

BEGIN END DataPoint.
