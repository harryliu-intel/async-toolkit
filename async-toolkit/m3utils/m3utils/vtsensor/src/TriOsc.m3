MODULE TriOsc;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT DataPoint;
IMPORT Text;
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
IMPORT OSError;
IMPORT P3P3Tbl;
IMPORT TriConfig;
IMPORT TriConfigSeq;
IMPORT Random;

<*FATAL Thread.Alerted*>
<*FATAL MapError.E*>

CONST TE = Text.Equal;
CONST LR = Fmt.LongReal;

PROCEDURE Calibrate(corner         : TEXT;
                    recs           : DataPointSeq.T;
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
          tempcoords := NEW(REF ARRAY OF Spline.Coord, seq.size());
        BEGIN
          FOR i := 0 TO seq.size() - 1 DO
            arr[i] := seq.get(i)
          END;
          
          (* ah why are we going through two data types... *)
          DataPointArraySort.Sort(arr^);
          
          FOR i := FIRST(arr^) TO LAST(arr^) DO
            tempcoords[i].x := arr[i].V;

            Debug.Out(F("tempcoords[%s].x = %s", Int(i), LR(tempcoords[i].x)));
            
            <*ASSERT i = 0 OR tempcoords[i].x > tempcoords[i-1].x*>
            tempcoords[i].y := arr[i].f;
          END;
          
          WITH spline = NEW(CubicSpline.T).init(tempcoords^) DO
            res.temps[k] := NEW(OscillatorTemp.T,
                                data      := arr,
                                tempcurve := spline,
                                minV      := arr[FIRST(arr^)].V,
                                maxV      := arr[LAST(arr^)].V)
          END
        END
      END
    END;

    RETURN res
  END Calibrate;

PROCEDURE MakeMesh(temp        : LONGREAL;
                   READONLY ot : ARRAY [0..2] OF OscillatorTemp.T;
                   N           : CARDINAL) : Mesh =
 
  VAR
    s0 := (ot[0].maxV - ot[0].minV) / FLOAT(N - 1, LONGREAL);
    s1 := (ot[1].maxV - ot[1].minV) / FLOAT(N - 1, LONGREAL);
    tempgrid := NEW(REF ARRAY OF ARRAY OF REF P3.T, N, N);
    volttbl := NEW(P3P3Tbl.Default).init();
  BEGIN
    Debug.Out(F("MakeMesh temp=%s", LR(temp)));
    Debug.Out(F("MakeMesh maxV0=%s minV0=%s", LR(ot[0].maxV), LR(ot[0].minV)));
    Debug.Out(F("MakeMesh maxV1=%s minV1=%s", LR(ot[1].maxV), LR(ot[1].minV)));
    Debug.Out(F("MakeMesh maxV2=%s minV2=%s", LR(ot[2].maxV), LR(ot[2].minV)));
    Debug.Out(F("s0 = %s, s1=%s", LR(s0), LR(s1)));

    FOR i0 := 0 TO N - 1 DO
      FOR i1 := 0 TO N - 1 DO
        tempgrid[i0,i1] := NIL;
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
              p^ := P3.T { ot[0].tempcurve.eval(v0),
                           ot[1].tempcurve.eval(v1),
                           ot[2].tempcurve.eval(v2) };
              tempgrid[i0, i1] := p;
              EVAL volttbl.put(p^, P3.T { v0, v1, v2 });
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
          WITH patch = Patch4 { tempgrid[i0    , i1    ],
                                tempgrid[i0    , i1 + 1],
                                tempgrid[i0 + 1, i1 + 1],
                                tempgrid[i0 + 1, i1    ] },
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

      RETURN Mesh { temp, triangles, volttbl }

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
                     N : CARDINAL) : REF ARRAY OF Mesh =
  VAR
    res := NEW(REF ARRAY OF Mesh, NUMBER(oscs[0].temps^));
  BEGIN
    FOR i := FIRST(oscs[0].temps^) TO LAST(oscs[0].temps^) DO
      res[i] := MakeMesh(calTemps[i],
                         ARRAY[0..2] OF OscillatorTemp.T { oscs[0].temps[i],
                                                           oscs[1].temps[i],
                                                           oscs[2].temps[i] },
                         N);

      <*FATAL OSError.E, Wr.Failure*>
      VAR
        p := res[i].triangles;
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


PROCEDURE Estimate(at         : P3.T;
                   tempMeshes : REF ARRAY OF Mesh;
                   Samples    : CARDINAL;
                   k          : LONGREAL) : TriConfig.T
  RAISES { NoData } =
  VAR
    sumweight, sumtemp := 0.0d0;
    sumvolts := P3.Zero;

    gotInterpolation := FALSE;

  BEGIN

    FOR j := 0 TO Samples - 1 DO
      TYPE
        Elt = LongrealPQ.Elt OBJECT
          intersection : IntersectionResult;
          temp : LONGREAL;
          volts : P3.T;
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
            mesh := tempMeshes[i];
            p    := mesh.triangles;
            temp := mesh.temp;
          BEGIN
            WHILE p # NIL DO
              WITH intersection = IntersectLine(at,
                                                dir,
                                                p.head) DO
                IF intersection.intersect THEN
                  Debug.Out(F("Intersection at temp %s at t=%s (u=%s v=%s)",
                              LR(temp),
                              LR(intersection.t),
                              LR(intersection.u),
                              LR(intersection.v)));

                  VAR
                    vv : ARRAY [0..2] OF P3.T;
                    iv : P3.T;
                  BEGIN
                    FOR i := FIRST(vv) TO LAST(vv) DO
                      WITH hadIt = mesh.volttbl.get(p.head[i], vv[i]) DO
                        <*ASSERT hadIt*>
                      END;
                      Debug.Out(F("Vertex v[%s] = %s", Int(i), P3.Format(vv[i])))
                    END;

                    (* are these barycentric coordinates right??? *)
                    
                    iv := P3.Plus(
                              P3.ScalarMul(intersection.v, vv[2]),
                              P3.Plus(P3.ScalarMul(intersection.u, vv[1]),
                                      P3.ScalarMul(1.0d0 - intersection.u - intersection.v,
                                                   vv[0])));
                    
                    Debug.Out(F("Interpolated v %s", P3.Format(iv)));
                  
                    results.insert(NEW(Elt,
                                       intersection := intersection,
                                       temp         := temp,
                                       volts        := iv,
                                       priority     := ABS(intersection.t)))
                    END;
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
            zfac := 1.0d0 / (y.intersection.t - x.intersection.t);
            ztemp := zfac * (y.intersection.t * x.temp - x.intersection.t * y.temp);

            zweight := Math.pow(ABS(y.intersection.t), k) *
                       Math.pow(ABS(x.intersection.t), k);

            zvolts := P3.ScalarMul(zfac,
                                   P3.Minus(P3.ScalarMul(y.intersection.t,
                                                         x.volts),
                                            P3.ScalarMul(x.intersection.t,
                                                         y.volts)));
          BEGIN
            gotInterpolation := TRUE;
            Debug.Out(F("interpolated temp %s volts %s weight %s",
                        LR(ztemp),
                        P3.Format(zvolts),
                        LR(zweight)));
            sumtemp   := sumtemp + ztemp * zweight;
            sumvolts  := P3.Plus(sumvolts, P3.ScalarMul(zweight, zvolts));
            sumweight := sumweight + zweight;
          END
        END
      END
    END(*ROF*);

    IF NOT gotInterpolation THEN
      RAISE NoData
    END;
    
    WITH avetemp = sumtemp/sumweight,
         avevolts = P3.ScalarMul(1.0d0/sumweight, sumvolts) DO

      <*ASSERT avetemp > -300.0d0*>
      Debug.Out(F("ave temp %s", LR(avetemp)));
      Debug.Out(F("ave volts %s", P3.Format(avevolts)));

      RETURN TriConfig.T { "xxxx",
                           avetemp,
                           avevolts,
                           at }
    END
  END Estimate;

PROCEDURE EvalEstimator(tempMeshes : REF ARRAY OF Mesh;
                        Samples : CARDINAL;
                        k : LONGREAL;
                        over : TriConfigSeq.T;
                        Tests : CARDINAL)  : ARRAY Dim OF DevRecord =
  VAR
    rand := NEW(Random.Default).init();
    n := over.size();
    dev : ARRAY Dim OF DevRecord;
    failures : CARDINAL := 0;
  BEGIN
    FOR i := 0 TO Tests - 1 DO
      TRY
        WITH p     = over.get(rand.integer(0, n - 1)),
             est   = Estimate(p.f, tempMeshes, Samples, k),
             delta = TriConfig.Minus(est, p) DO
          Debug.Out(F("p %s, est %s, delta %s",
                      TriConfig.Format(p),
                      TriConfig.Format(est),
                      TriConfig.Format(delta)));
          
          Dev(delta.temp, dev[Dim.Temperature]);
          Dev(delta.V[0], dev[Dim.Voltage]);
          Dev(delta.V[1], dev[Dim.Voltage]);
          Dev(delta.V[2], dev[Dim.Voltage]);
        END
      EXCEPT
        NoData => INC(failures)
      END
    END;
    Debug.Out(F("temp dev %s", FmtDev(dev[Dim.Temperature])));
    Debug.Out(F("volt dev %s", FmtDev(dev[Dim.Voltage])));
    RETURN dev
  END EvalEstimator;

PROCEDURE Dev(dev     : LONGREAL;
              VAR rec : DevRecord) =
  BEGIN
    INC(rec.n);
    rec.maxDev := MAX(rec.maxDev, ABS(dev));
    rec.sumAbs := rec.sumAbs + ABS(dev);
    rec.sumSq  := rec.sumSq  + dev * dev;
  END Dev;

PROCEDURE FmtDev(READONLY rec : DevRecord) : TEXT =
  BEGIN
    RETURN F("%s recs : maxDev %s, aveAbsDev %s, sDev %s",
             Int(rec.n),
             LR(rec.maxDev),
             LR(rec.sumAbs / FLOAT(rec.n, LONGREAL)),
             LR(Math.sqrt(rec.sumSq) / FLOAT(rec.n - 1, LONGREAL)))
  END FmtDev;
  
BEGIN END TriOsc.
