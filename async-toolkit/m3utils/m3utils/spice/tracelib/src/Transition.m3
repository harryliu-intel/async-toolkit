(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Transition;
IMPORT LongrealType AS LR;
IMPORT Word;
FROM Fmt IMPORT F, Style, LongReal, Int;

PROCEDURE CompareByTime(READONLY a, b : T) : CompareResult =
  BEGIN RETURN LR.Compare(a.at, b.at) END CompareByTime;

PROCEDURE CompareBySlew(READONLY a, b : T) : CompareResult =
  BEGIN RETURN LR.Compare(a.slew, b.slew) END CompareBySlew;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  (* it's generally not interesting to have more than one transition at a 
     given time since these objects are not keyed by waveform id *)
  BEGIN RETURN LR.Hash(a.at) END Hash;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("{ @ %s : %3s slew %s }",
             LongReal(a.at,   style := Style.Sci, prec := 4),
             Int(a.dir),
             LongReal(a.slew, style := Style.Sci, prec := 4))
  END Format;

BEGIN END Transition.
