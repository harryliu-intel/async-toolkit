MODULE TriOsc;
IMPORT Rd;
IMPORT Json;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT DataPoint;
FROM CitTextUtils IMPORT HaveSuffix, RemoveSuffix;
IMPORT SIsuffix;
IMPORT Text;
IMPORT Scan;
IMPORT DataPointSeq;
IMPORT Spline;
IMPORT CubicSpline;
IMPORT DataPointArraySort;
IMPORT OscillatorTemp;
IMPORT Oscillator;
IMPORT P3;
IMPORT Triangle3, Triangle3List;
FROM Triangle3 IMPORT IntersectionResult, IntersectLine;
IMPORT FileWr, Wr;
IMPORT LongrealPQ;
IMPORT Math;
IMPORT Thread;
IMPORT MapError;
IMPORT Lex, FloatMode;
IMPORT OSError;

<*FATAL Thread.Alerted*>
<*FATAL MapError.E*>

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

PROCEDURE Calibrate(corner : TEXT;
                    recs : DataPointSeq.T;
                    READONLY temps : ARRAY OF LONGREAL) : Oscillator.T =
  VAR
    res := NEW(Oscillator.T,
               temps := NEW(REF ARRAY OF OscillatorTemp.T, NUMBER(temps)));
  BEGIN
    FOR k := FIRST(temps) TO LAST(temps) DO
      VAR temp := temps[k];
          seq := NEW(DataPointSeq.T).init();
      BEGIN
        Debug.Out(F("Attempting calibration at %s", LR(temp)));

        FOR j := 0 TO recs.size() - 1 DO
          WITH rec = recs.get(j) DO
            IF rec.temp = temp AND TE(rec.corner, corner) THEN
              seq.addhi(rec)
            END
          END
        END;
        
        Debug.Out(F("%sC, %s points", LR(temp), Int(seq.size())));

        VAR
          arr := NEW(REF ARRAY OF DataPoint.T, seq.size());
          coords := NEW(REF ARRAY OF Spline.Coord, seq.size());
        BEGIN
          FOR i := 0 TO seq.size() - 1 DO
            arr[i] := seq.get(i)
          END;
          
          (* ah why are we going through two data types... *)
          DataPointArraySort.Sort(arr^);
          
          FOR i := FIRST(arr^) TO LAST(arr^) DO
            coords[i].x := arr[i].V;

            Debug.Out(F("coords[%s].x = %s", Int(i), LR(coords[i].x)));
            
            <*ASSERT i = 0 OR coords[i].x > coords[i-1].x*>
            coords[i].y := arr[i].f;
          END;
          
          WITH spline = NEW(CubicSpline.T).init(coords^) DO
            res.temps[k] := NEW(OscillatorTemp.T,
                                data  := arr,
                                curve := spline,
                                minV  := arr[FIRST(arr^)].V,
                                maxV  := arr[LAST(arr^)].V)
          END
        END
      END
    END;

    RETURN res
  END Calibrate;

PROCEDURE MakeMesh(temp : LONGREAL;
                   READONLY ot : ARRAY [0..2] OF OscillatorTemp.T;
                   N : CARDINAL) : Triangle3List.T =
  VAR
    s0 := (ot[0].maxV - ot[0].minV) / FLOAT(N - 1, LONGREAL);
    s1 := (ot[1].maxV - ot[1].minV) / FLOAT(N - 1, LONGREAL);
    grid := NEW(REF ARRAY OF ARRAY OF REF P3.T, N, N);
  BEGIN
    Debug.Out(F("MakeMesh temp=%s", LR(temp)));
    Debug.Out(F("MakeMesh maxV0=%s minV0=%s", LR(ot[0].maxV), LR(ot[0].minV)));
    Debug.Out(F("MakeMesh maxV1=%s minV1=%s", LR(ot[1].maxV), LR(ot[1].minV)));
    Debug.Out(F("MakeMesh maxV2=%s minV2=%s", LR(ot[2].maxV), LR(ot[2].minV)));
    Debug.Out(F("s0 = %s, s1=%s", LR(s0), LR(s1)));

    FOR i0 := 0 TO N - 1 DO
      FOR i1 := 0 TO N - 1 DO
        grid[i0,i1] := NIL
      END
    END;

    FOR i0 := 0 TO N - 1 DO
      FOR i1 := 0 TO N - 1 DO
        WITH v0 = ot[0].minV + s0 * FLOAT(i0, LONGREAL),
             v1 = ot[1].minV + s1 * FLOAT(i1, LONGREAL),
             v2 = v0 + v1 DO

          Debug.Out(F("v0 = %s, v1=%s, v2=%s", LR(v0), LR(v1), LR(v2)));
          
          IF v2 >= ot[2].minV AND v2 <= ot[2].maxV THEN
            (* point is OK *)
            WITH p = NEW(REF P3.T) DO
              p^ := P3.T { ot[0].curve.eval(v0),
                           ot[1].curve.eval(v1),
                           ot[2].curve.eval(v2) };
              grid[i0, i1] := p;
              
              Debug.Out(F("Mesh point %s", P3.Format(p^)))
            END
          END
          
        END
      END(*ROF*)
    END(*ROF*);

    (* at this point we have filled in the rectangular mesh *)

    <*FATAL OSError.E, Wr.Failure*>
    VAR
      triangles : Triangle3List.T := NIL;
      wr := FileWr.Open(F("patch.%s.dat", LR(temp)));
    BEGIN
      FOR i0 := 0 TO N - 2 DO
        FOR i1 := 0 TO N - 2 DO
          WITH patch = Patch4 { grid[i0    , i1    ],
                                grid[i0    , i1 + 1],
                                grid[i0 + 1, i1 + 1],
                                grid[i0 + 1, i1    ] },
               nils = CountNils(patch) DO

            Wr.PutText(wr, FmtPatch4Gnu(patch));
            
            IF nils = 0 THEN
              MakeTriangles(patch, triangles)
            ELSIF nils = 1 THEN
              MakeTriangle(patch, triangles)
            END
          END
        END
      END(*ROF*);
      Wr.Close(wr);
      
      VAR
        p := triangles;
      BEGIN
        WHILE p # NIL DO
          Debug.Out(F("Triangle %s", Triangle3.Format(p.head)));
          p := p.tail
        END
      END;
      RETURN triangles
    END
    
  END MakeMesh;

PROCEDURE CountNils(patch : Patch4) : CARDINAL =
  VAR
    res := 0;
  BEGIN
    FOR i := FIRST(patch) TO LAST(patch) DO
      IF patch[i] = NIL THEN INC(res) END
    END;
    RETURN res
  END CountNils;
  
PROCEDURE MakeTriangle(patch : Patch4; VAR tr : Triangle3List.T) =
  (* REQUIRES CountNils(patch) = 1 *)
  VAR
    j := 0;
    res : Triangle3.T;
  BEGIN
    FOR i := FIRST(Corner) TO LAST(Corner) DO
      IF patch[i] # NIL THEN
        res[j] := patch[i]^;
        INC(j)
      END
    END;
    <*ASSERT j=3*>
    tr := Triangle3List.Cons(res, tr)
  END MakeTriangle;

PROCEDURE MakeTriangles(patch : Patch4; VAR tr : Triangle3List.T) =
  VAR
    cands : ARRAY Corner OF Triangle3.T;
    p : Patch4;
    q : Triangle3List.T;
    skinny : Corner;
    maxCos := 0.0d0;
  BEGIN
    Debug.Out(F("MakeTriangles(%s):\n", FmtPatch4Gnu(patch)));
    FOR c := FIRST(Corner) TO LAST(Corner) DO
      p := patch;
      p[c] := NIL;
      MakeTriangle(p, q);
      cands[c] := q.head
    END;
    FOR c := FIRST(Corner) TO LAST(Corner) DO
      WITH thisCos = Triangle3.MaxCos(cands[c]) DO
        IF thisCos > maxCos THEN
          skinny := c;
          maxCos := thisCos
        END
      END
    END;

    (* we do not want skinny or skinny+2, we want skinny +/- 1*)
    Debug.Out(F("Skinny is %s", CornerNames[skinny]));
    WITH a = VAL((ORD(skinny) - 1) MOD NUMBER(Corner), Corner),
         b = VAL((ORD(skinny) + 1) MOD NUMBER(Corner), Corner) DO
      tr := Triangle3List.Cons(cands[a], tr);
      tr := Triangle3List.Cons(cands[b], tr);
      Debug.Out(F("Adding triangle a %s %s",
                  CornerNames[a],
                  Triangle3.Format(cands[a])));
      Debug.Out(F("Adding triangle b %s %s",
                  CornerNames[b],
                  Triangle3.Format(cands[b])));
    END
  END MakeTriangles;

TYPE Corner = { ll, ul, ur, lr }; (* clockwise *)
     Patch4 = ARRAY Corner OF REF P3.T;

CONST CornerNames = ARRAY Corner OF TEXT { "LL",
                                           "UL",
                                           "UR",
                                           "LR" };

PROCEDURE FmtPatch4Gnu(READONLY p : Patch4) : TEXT =
  VAR
    s := "";
    f : REF P3.T := NIL;
  BEGIN
    FOR i := FIRST(p) TO LAST(p) DO
      IF p[i] # NIL THEN
        IF f # NIL THEN f := p[i] END;
        s := s & P3.FormatGnu(p[i]^) & "\n"
      END
    END;
    IF f # NIL THEN s := s & P3.FormatGnu(f^) & "\n"  END;
    s := s & "\n";
    RETURN s
  END FmtPatch4Gnu;
     
PROCEDURE MakeMeshes(oscs : ARRAY [0..2] OF Oscillator.T;
                     READONLY calTemps : ARRAY OF LONGREAL;
                     N : CARDINAL) : REF ARRAY OF Triangle3List.T =
  VAR
    res := NEW(REF ARRAY OF Triangle3List.T, NUMBER(oscs[0].temps^));
  BEGIN
    FOR i := FIRST(oscs[0].temps^) TO LAST(oscs[0].temps^) DO
      res[i] := MakeMesh(calTemps[i],
                         ARRAY[0..2] OF OscillatorTemp.T { oscs[0].temps[i],
                                                           oscs[1].temps[i],
                                                           oscs[2].temps[i] },
                         N);

      <*FATAL OSError.E, Wr.Failure*>
      VAR
        p := res[i];
        wr := FileWr.Open(F("mesh.%s.dat", LR(calTemps[i])));
      BEGIN
        WHILE p # NIL DO
          Wr.PutText(wr, Triangle3.FormatGnu(p.head));
          p := p.tail
        END;
        Wr.Close(wr)
      END
    END;
    RETURN res
  END MakeMeshes;


PROCEDURE Estimate(at : P3.T;
                   tempMeshes : REF ARRAY OF Triangle3List.T;
                   READONLY calTemps : ARRAY OF LONGREAL;
                   Samples : CARDINAL;
                   k : LONGREAL) : LONGREAL =
  VAR
    sumweight, sumtemp := 0.0d0;

  BEGIN

    FOR j := 0 TO Samples - 1 DO
      TYPE
        Elt = LongrealPQ.Elt OBJECT
          intersection : IntersectionResult;
          temp : LONGREAL;
        END;
      VAR
        dir := P3.RandomDirection();
        results := NEW(LongrealPQ.Default).init();
      BEGIN
        Debug.Out(F("working on %s, probing %s",
                    P3.Format(at),
                    P3.Format(dir)));
        FOR i := FIRST(tempMeshes^) TO LAST(tempMeshes^) DO
          VAR
            p := tempMeshes[i];
          BEGIN
            WHILE p # NIL DO
              WITH intersection = IntersectLine(at,
                                                dir,
                                                p.head) DO
                IF intersection.intersect THEN
                  Debug.Out(F("Intersection at temp %s at t=%s",
                              LR(calTemps[i]),
                              LR(intersection.t)));
                  results.insert(NEW(Elt,
                                     intersection := intersection,
                                     temp := calTemps[i],
                                     priority := ABS(intersection.t)));
                  EXIT
                END
              END;
              p := p.tail
            END
          END
        END;

        IF results.size() >= 2 THEN
          (* we have enough data to proceed with interpolation *)
          <*FATAL LongrealPQ.Empty*>
          VAR
            x : Elt := results.deleteMin();
            y : Elt := results.deleteMin();
            ztemp := (y.intersection.t * x.temp - x.intersection.t * y.temp) /
            (y.intersection.t - x.intersection.t);
            zweight := Math.pow(ABS(y.intersection.t), k) *
                       Math.pow(ABS(x.intersection.t), k);
          BEGIN
            Debug.Out(F("interpolated temp %s weight %s",
                        LR(ztemp),
                        LR(zweight)));
            sumtemp := sumtemp + ztemp * zweight;
            sumweight := sumweight + zweight;
          END
        END
      END
    END(*ROF*);
    Debug.Out(F("ave temp %s", LR(sumtemp/sumweight)));
    RETURN sumtemp/sumweight
  END Estimate;

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
  
BEGIN END TriOsc.
