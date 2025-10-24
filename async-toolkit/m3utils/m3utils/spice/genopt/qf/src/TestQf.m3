(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TestQf;

IMPORT QuadraticFit;
IMPORT Debug;
IMPORT LRVector;
FROM Fmt IMPORT F, LongReal, Int;
IMPORT LRMatrix2 AS M;
FROM GenOpt IMPORT FmtP;

CONST LR = LongReal;

PROCEDURE Func(p : LRVector.T) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      WITH d = p[i] - 2.0d0 DO
        res := res + d * d
      END
    END;
    RETURN res
  END Func;

PROCEDURE DoIt() =
  VAR
    qf : QuadraticFit.T;

    p := NEW(LRVector.T, 2);
  BEGIN
    Debug.Out("TestQf.DoIt()");

    qf := NEW(QuadraticFit.T).init(NUMBER(p^), 1.0d0);

    M.ZeroV(p^);

    qf.addPoint(p, Func(p));

    (* eval at a buncha points *)
    FOR h := 1 TO 5 DO
      FOR i := FIRST(p^) TO LAST(p^) DO
        p[i] := 0.5d0 * FLOAT(h, LONGREAL);
        qf.addPoint(p, Func(p))
      END
    END;

    WITH fs = qf.getState() DO
      Debug.Out(F("TestQf : qf.getState() = %s",
                  FmtP(fs)));

      Debug.Out(F("TestQf : qf.evalState(%s) = %s",
                  FmtP(fs), LR(qf.evalState(fs))));

      VAR
        x := LRVector.Copy(fs);
        y : LRVector.T;
      BEGIN
        FOR i := -1 TO 1 DO
          FOR j := -1 TO 1 DO
            y := LRVector.Copy(x);
            y[0] := y[0] + FLOAT(i, LONGREAL) * 0.001d0;
            y[1] := y[1] + FLOAT(j, LONGREAL) * 0.001d0;
            Debug.Out(F("TestQf [ %s %s ] : qf.evalState(%s) = %s",
                        Int(i), Int(j),
                        FmtP(y), LR(qf.evalState(y))));
          END
        END
      END
    END;
      
    
    WITH q = ARRAY OF LONGREAL { 2.0d0, 2.0d0, 1.0d0, 0.0d0, 1.0d0, 0.0d0 },
         qa = NEW(LRVector.T, NUMBER(q)) DO
      qa^ := q;
      Debug.Out(F("TestQf : qf.evalState(%s) = %s",
                  FmtP(qa), LR(qf.evalState(qa))));

      IF FALSE THEN
        qa[0] := 0.6827095300026415d0;
        qa[1] := 1.936747089419486d0;
        qa[2] := -4.193611435233667d-6;
        qa[3] := -1.37344847273883d0;
        qa[4] := -0.033367444728433414d0;
        qa[5] := 0.015177215675018868d0;
        
        Debug.Out(F("TestQf : qf.evalState(%s) = %s",
                    FmtP(qa), LR(qf.evalState(qa))));
      END
    END
  END DoIt;

BEGIN END TestQf.
