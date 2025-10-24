(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ArithProbability;
FROM Fmt IMPORT Int, F;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN F("{lo=%s hi=%s cnt=%s}", Int(t.lo), Int(t.hi), Int(t.count))
  END Format;

BEGIN END ArithProbability.
