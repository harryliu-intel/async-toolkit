(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Latches;
IMPORT Trace;
IMPORT Debug;
FROM Fmt IMPORT F;
IMPORT Text;
IMPORT CitTextUtils;

PROCEDURE IsQNode(trace          : Trace.T;
                  idx            : CARDINAL;
                  VAR latchNodes : LatchNodes) : BOOLEAN =
  
  PROCEDURE AliasHasSuffix(idx : CARDINAL;
                           sfx : TEXT;
                           VAR pfx : TEXT) : BOOLEAN=
    VAR iter := trace.getAliases(idx).iterate();
        a : TEXT;
    BEGIN
      WHILE iter.next(a) DO
        IF FALSE THEN
          Debug.Out(F("seeking suffix %s in %s", sfx, a))
        END;
        IF CitTextUtils.HaveSuffix(a, sfx) THEN
          pfx := Text.Sub(a, 0, Text.Length(a) - Text.Length(sfx));
          RETURN TRUE
        END
      END;
      RETURN FALSE
    END AliasHasSuffix;

  PROCEDURE Exists(nn : TEXT) : BOOLEAN =
    VAR
      dummy : CARDINAL;
    BEGIN
      RETURN trace.getNodeIdx(nn, dummy)
    END Exists;

  VAR
    pfx : TEXT;
  BEGIN
    IF AliasHasSuffix(idx, ".Q", pfx) THEN
      WITH clkNm = pfx & ".CLK",
           dNm   = pfx & ".D",
           qNm   = pfx & ".Q" DO
        
        IF Exists(pfx & ".D") AND Exists(pfx & ".CLK") THEN
          latchNodes := LatchNodes { clkNm, dNm, qNm };
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END IsQNode;

BEGIN END Latches.
