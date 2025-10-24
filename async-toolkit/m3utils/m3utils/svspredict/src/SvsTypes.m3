(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SvsTypes;
FROM Fmt IMPORT LongReal, F;

CONST LR = LongReal;
      
PROCEDURE FmtCornerData(READONLY cd : CornerData) : TEXT =
  BEGIN
    RETURN F("CornerData{ vtiming=%s vpower=%s sigma=%s }",
             LR(cd.vtiming), LR(cd.vpower), LR(cd.sigma))
  END FmtCornerData;

BEGIN END SvsTypes.
