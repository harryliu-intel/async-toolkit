(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TriData;
IMPORT Json, DataPointSeq, DataPoint;
IMPORT Rd;
IMPORT Lex, FloatMode;
IMPORT SIsuffix;
FROM CitTextUtils IMPORT HaveSuffix, RemoveSuffix;
IMPORT Scan, Debug;
IMPORT Text;
IMPORT Fmt;
FROM Fmt IMPORT F, Int;
IMPORT TriConfigList;
IMPORT TriConfigSeq;
IMPORT TriConfig;
IMPORT P3;
IMPORT LRDataPointSeqTbl;

CONST TE = Text.Equal;
CONST LR = Fmt.LongReal;

PROCEDURE DoOne(j : Json.T;
                depth : CARDINAL;
                VAR recs : DataPointSeq.T;
                VAR cur : DataPoint.T)
  RAISES { SyntaxError } =  
  VAR
    iter := j.iterate();
    nm : TEXT;
    val : Json.T;
  BEGIN
    TRY
    WHILE iter.next(nm, val) DO
      WITH k = val.kind() DO
        (*
        Debug.Out(F("depth %s child %s kind %s", Int(depth), nm, Json.NK[k]));
        *)
        IF    depth = 1 THEN
          cur.corner := nm
        ELSIF HaveSuffix(nm, "V") THEN
          WITH val = RemoveSuffix(nm, "V") DO
            cur.V := SIsuffix.LongReal(val)
          END
        ELSIF HaveSuffix(nm, "C") THEN
          WITH val = RemoveSuffix(nm, "C") DO
            cur.temp := SIsuffix.LongReal(val)
          END
        ELSIF TE(nm, "frequency") THEN
          cur.f := 1.0d06 * Scan.LongReal(val.value());
          recs.addhi(cur)
        ELSE
          Debug.Error("Unhandled node case.")
        END;
          
        IF k = Json.NodeKind.nkObject THEN
          DoOne(val, depth + 1, recs, cur)
        END
      END
    END
    EXCEPT
      FloatMode.Trap, Lex.Error, SIsuffix.UnknownSuffix =>
      RAISE SyntaxError
    END
  END DoOne;

PROCEDURE LoadJson(rd : Rd.T) : DataPointSeq.T
  RAISES { SyntaxError, Json.E } =
  VAR
    json := Json.ParseStream(rd);
    recs := NEW(DataPointSeq.T).init();
    cur : DataPoint.T;
  BEGIN
    DoOne(json, 1, recs, cur);
    RETURN recs
  END LoadJson;

PROCEDURE FilterCorner(s0 : DataPointSeq.T; corner : TEXT) : DataPointSeq.T =
  VAR
    res := NEW(DataPointSeq.T).init();
  BEGIN
    FOR i := 0 TO s0.size() - 1 DO
      WITH d = s0.get(i) DO
        IF TE(d.corner, corner) THEN
          res.addhi(d)
        END
      END
    END;
    RETURN res
  END FilterCorner;

PROCEDURE CollateTemp(seq : DataPointSeq.T) : LRDataPointSeqTbl.T =
  VAR
    res := NEW(LRDataPointSeqTbl.Default).init();
    ref : DataPointSeq.T;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH d = seq.get(i) DO
        IF NOT res.get(d.temp, ref) THEN
          ref := NEW(DataPointSeq.T).init();
          EVAL res.put(d.temp, ref)
        END;
        ref.addhi(d)
      END
    END;
    RETURN res
  END CollateTemp;
  
PROCEDURE TraverseData(iseq   : DataPointSeq.T;
                       corner : TEXT) : TriConfigSeq.T =
  VAR
    a, b, c : DataPoint.T;
    res := NEW(TriConfigSeq.T).init();
    seq := FilterCorner(iseq, corner);
    tempTbl := CollateTemp(seq);
    bseq : DataPointSeq.T;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      a := seq.get(i);
      WITH hadIt = tempTbl.get(a.temp, bseq) DO
        <*ASSERT hadIt*>
      END;
      FOR j := 0 TO bseq.size() - 1 DO
        b := bseq.get(j);
        IF a.temp = b.temp THEN
          FOR k := 0 TO bseq.size() - 1 DO
            c := bseq.get(k);
            IF c.temp = a.temp AND ABS(a.V + b.V - c.V) < 1.0d-6 THEN
              (* a b c are a triple *)
              
              (*
              Debug.Out(F("Triple corner %s temp %s", corner, LR(a.temp)));
              Debug.Out(F("v0 %s f0 %s", LR(a.V), LR(a.f)));
              Debug.Out(F("v1 %s f1 %s", LR(b.V), LR(b.f)));
              Debug.Out(F("v2 %s f2 %s", LR(c.V), LR(c.f)));
              *)
              
              res.addhi(
                  TriConfig.T { corner, a.temp,
                                P3.T { a.V, b.V, c.V },
                                P3.T { a.f, b.f, c.f } }
                  )
            END
          END
        END
      END
    END;
    RETURN res
  END TraverseData;
  
BEGIN END TriData.
