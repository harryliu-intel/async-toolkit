(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Power3;
FROM Fmt IMPORT F, LongReal;

CONST LR = LongReal;
  
PROCEDURE DebugFmt(READONLY p3 : T) : TEXT =
  BEGIN
    RETURN F("Power3 { dynP = %s, lkgP = %s, totP = %s }",
             LR(p3.dynP), LR(p3.lkgP), LR(p3.totP))
  END DebugFmt;
  
BEGIN END Power3.
