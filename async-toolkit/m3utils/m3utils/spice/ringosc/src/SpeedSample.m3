(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpeedSample;
IMPORT LongrealType;
IMPORT Cardinal;
FROM Fmt IMPORT F, Int;
IMPORT Fmt;

CONST LR = Fmt.LongReal;

PROCEDURE CompareBySpeed(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN Cardinal.Compare(a.speed, b.speed)
  END CompareBySpeed;

PROCEDURE CompareByPctFromPrev(READONLY a, b : T) : [-1..1] =
  BEGIN
    RETURN LongrealType.Compare(a.pctFromPrev, b.pctFromPrev)
  END CompareByPctFromPrev;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("speed %s cyc %s freq %s delcyc %s pctFromPrev %s",
             Int(a.speed),
             LR(a.cyc),
             LR(a.freq),
             LR(a.delcyc),
             LR(a.pctFromPrev))
  END Format;

BEGIN END SpeedSample.
