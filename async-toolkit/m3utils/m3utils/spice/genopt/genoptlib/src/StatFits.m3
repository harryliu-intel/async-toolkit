MODULE StatFits;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT MultiEvalLR AS MultiEval;
IMPORT LRRegression AS Regression;
IMPORT LongrealType;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal, FN;
IMPORT LRVector;
IMPORT PointMetric;
IMPORT Math;
IMPORT PointMetricArraySort;
IMPORT Wx;
IMPORT StatFitsSeq;
IMPORT StatFitsArraySort;
IMPORT Matrix;

FROM SurfaceRep IMPORT Qdofs, Ldofs, ComputeIndepsL, ComputeIndepsQ,
                       ComputeL, L2Q, FmtQ;

CONST LR = LongReal;
TYPE  TA = ARRAY OF TEXT;
      
PROCEDURE Rss(a, b : REF M.M) : LONGREAL =
  BEGIN
    RETURN M.SumDiffSqM(a^, b^)
  END Rss;

PROCEDURE Vdist(a, b : LRVector.T) : LONGREAL =
  BEGIN
    RETURN Math.sqrt(M.SumDiffSqV(a^, b^))
  END Vdist;

VAR Pi     := 4.0d0 * Math.atan(1.0d0);
    Log2Pi := Math.log(2.0d0 * Pi);

PROCEDURE Format(READONLY t : T ) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, t.debug);
    Wx.PutChar(wx, '\n');

    Wx.PutText  (wx, "mu    = " & FmtQ(t.n, t.bmu));
    Wx.PutChar(wx, '\n');

    Wx.PutText  (wx, "sigma = " & FmtQ(t.n, t.bsigma));
    Wx.PutChar(wx, '\n');
    
    Wx.PutText(wx, F("validation log mu  likelihood = %s",
                     LR(t.lmu)));
    Wx.PutChar(wx, '\n');
    
    Wx.PutText(wx, F("validation log sig likelihood = %s", LR(t.lsig)));
    Wx.PutChar(wx, '\n');
    
    Wx.PutText(wx, F("validation log likelihood     = %s nlf=%s l/nlf=%s",
                     LR(t.l), LR(t.nlf), LR(t.l/t.nlf)));
    Wx.PutChar(wx, '\n');
    
    Wx.PutText(wx, F("total log likelihood          = %s        nllf=%s ll/nllf=%s", LR(t.ll), LR(t.nllf), LR(t.ll/t.nllf)));
    Wx.PutChar(wx, '\n');

    Wx.PutText(wx, "Ranking " & FmtRanking(t.rank));
    Wx.PutChar(wx, '\n');

    RETURN Wx.ToText(wx)
  END Format;

PROCEDURE FmtRanking(ra : ARRAY Ranking OF CARDINAL) : TEXT =
  BEGIN
    RETURN F("%s %s %s",
             Int(ra[Ranking.MeanValL]),
             Int(ra[Ranking.MeanAllL]),
             Int(ra[Ranking.SumAbsLin]))
  END FmtRanking;
  
PROCEDURE Attempt(p           : LRVector.T;
                  parr        : REF ARRAY OF PointMetric.T;
                  selectByAll : BOOLEAN ) : T
  RAISES { Matrix.Singular } =


  PROCEDURE DoFit(READONLY parr, varr : ARRAY OF PointMetric.T)
    RAISES { Matrix.Singular } =

    PROCEDURE DoPoint(READONLY v : PointMetric.T; sMu, sSig : LONGREAL) =
      BEGIN
        WITH
          (* data *)
          dmu  = MultiEval.Mean(v.result), 
          dsig = MultiEval.Sdev(v.result),
          
          ns   = v.result.n,
          nsf  = FLOAT(ns, LONGREAL),
          
          (* predicted *)
          pmu  = ComputeL(v.p, rmu.b),
          psig = ComputeL(v.p, rsigma.b),
          
          (* likelihoods *)
          lmu  = nsf * LogLikelihood(dmu , pmu,  sMu),
          lsig = nsf * LogLikelihood(dsig, psig, sSig),
          l    = lmu + lsig DO
          
          sumlMu  := sumlMu  + lmu;
          sumlSig := sumlSig + lsig;
          sump    := sump    + ns;

          pq.insert(NEW(PElt,
                        priority := l,
                        p        := v.p,
                        result   := v.result,
                        pmu      := pmu,
                        psig     := psig,
                        lmu      := lmu,
                        lsig     := lsig))
        END
      END DoPoint;

    VAR
      m  := NUMBER(parr);
      mf := FLOAT(m, LONGREAL);

      n := NUMBER(parr[FIRST(parr)].p^);
      
      qdofs  := Qdofs(n);
      ldofs  := Ldofs(n);
      
      xquad  := NEW(REF M.M, m, qdofs);
      xlin   := NEW(REF M.M, m, ldofs);

      ymu    := NEW(REF M.M, m, 1);
      ysigma := NEW(REF M.M, m, 1);
      w      := NEW(REF M.M, m, m);

      ymuHat,
      ysigmaHat : REF M.M;

      rmu    := NEW(Regression.T);
      rsigma := NEW(Regression.T);

      sumlMu, sumlSig := 0.0d0;
      sump            := 0;
      pq              := NEW(LongrealPQ.Default).init();

      mostDistant     := varr[LAST(varr)].p;
      radius          := Vdist(p, mostDistant);
      
    BEGIN
      
      M.Zero(w^);
      FOR i := 0 TO m - 1 DO
        WITH rec = parr[i] DO
          ComputeIndepsQ(rec.p, i, xquad^);
          ComputeIndepsL(rec.p, i, xlin^);

          ymu   [i, 0] := MultiEval.Mean   (rec.result);
          ysigma[i, 0] := MultiEval.Sdev   (rec.result);
          
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);
 
      Regression.Run(xlin,
                     ymu   , ymuHat   , FALSE, rmu   , 0.0d0, W := w);
      Regression.Run(xlin,
                     ysigma, ysigmaHat, FALSE, rsigma, 0.0d0, W := w);

      WITH rssMu  = Rss(ymu   , ymuHat),
           sMu    = Math.sqrt(rssMu / mf),
           rssSig = Rss(ysigma, ysigmaHat),
           sSig   = Math.sqrt(rssSig / mf),
           debug  =FN("AttemptStatFits2 m=%s radius=%s rssMu=%s rssSig=%s ; sMu=%s sSig=%s",
                     TA{Int(m), LR(radius), LR(rssMu), LR(rssSig), LR(sMu), LR(sSig)}) DO
        
        (* likelihood on the validation set *)
        FOR i := FIRST(varr) TO LAST(varr) DO
          WITH v    = varr[i] DO
            DoPoint(v, sMu, sSig)
          END
        END;

        VAR
          l     := sumlMu + sumlSig;
          nlf   := FLOAT(sump, LONGREAL); (* measurements in l *)

          lmu   := sumlMu;
          lsig  := sumlSig;

          ll    : LONGREAL;
          nllf  : LONGREAL;
        BEGIN
          (* do the main points too *)
          FOR i := FIRST(parr) TO LAST(parr) DO
            WITH v = parr[i] DO
              DoPoint(v, sMu, sSig)
            END
          END;

          ll := sumlMu + sumlSig;
          nllf   := FLOAT(sump, LONGREAL);

          WITH fit =  T { debug,
                          thefits.size(),
                          n,
                          l,
                          lmu, lsig,
                          nlf,
                          L2Q(n, rmu.b^),
                          L2Q(n, rsigma.b^),
                          ll,
                          nllf,
                          pts := pq,
                          evals := sump,
                          rank := ARRAY Ranking OF CARDINAL { LAST(CARDINAL), .. }
            } DO
            thefits.addhi(fit)
          END
        END;
      END
      
    END DoFit;

  PROCEDURE ComparePMByDistance(READONLY a, b : PointMetric.T) : [-1 .. 1] =
    BEGIN
      WITH ad = Vdist(a.p, p),
           bd = Vdist(b.p, p) DO
        RETURN LongrealType.Compare(ad, bd)
      END
    END ComparePMByDistance;
    
  CONST
    LeaveOut = 16;
    Comparer = ARRAY Ranking OF CmpProc { CompareByMeanValLikelihood,
                                          CompareByMeanAllLikelihood,
                                          CompareBySumAbsLinCoeff };
  VAR
    m := 2;
    max := NUMBER(parr^) - LeaveOut;

    bestr  := LAST(CARDINAL);
    bestfits : T;
    thefits := NEW(StatFitsSeq.T).init();

    fa : REF ARRAY OF T;
  BEGIN
    PointMetricArraySort.Sort(parr^, cmp := ComparePMByDistance);
    LOOP
      DoFit(SUBARRAY(parr^,           0, MIN(max, m)),
            SUBARRAY(parr^, MIN(max, m), LeaveOut));
      IF m >= max THEN
        EXIT
      END;
      m := m * 2
    END;

    (* we have all the fits in thefits *)
    fa := NEW(REF ARRAY OF T, thefits.size());

    FOR i := 0 TO thefits.size() - 1 DO
      fa[i] := thefits.get(i)
    END;

    FOR r := FIRST(Ranking) TO LAST(Ranking) DO
      StatFitsArraySort.Sort(fa^, Comparer[r]);
      FOR i := FIRST(fa^) TO LAST(fa^) DO
        fa[i].rank[r] := i;
        thefits.put(fa[i].i, fa[i]) (* synchronize *)
      END
    END;

    FOR i := 0 TO thefits.size() - 1 DO
      VAR
        f  := thefits.get(i);
        sr : CARDINAL;
      BEGIN
        IF selectByAll THEN
          sr := f.rank[Ranking.MeanAllL] + f.rank[Ranking.SumAbsLin]
        ELSE
          sr := f.rank[Ranking.MeanValL] + f.rank[Ranking.SumAbsLin]
        END;
        
        Debug.Out(Format(f));
        
        IF sr < bestr THEN
          bestfits := f;
          bestr := sr
        END
      END
    END;

    Debug.Out("BEST RANKED FIT\n" & Format(bestfits));
    
    RETURN bestfits
  END Attempt;

PROCEDURE LogLikelihood(x, mu, sigma : LONGREAL) : LONGREAL =
  (* likelihood (density) of x given it is ~N(mu, sigma) *)
  CONST
    log = Math.log;
  BEGIN
    WITH t1 = -0.5d0 * Log2Pi,
         t2 = -log(sigma),
         dev = x - mu,
         t3 = -0.5d0 / sigma / sigma * dev * dev DO
      RETURN t1 + t2 + t3
    END
  END LogLikelihood;

PROCEDURE VarM(a : REF M.M) : LONGREAL =
  BEGIN
    RETURN M.DevSqM(a^)
  END VarM;

PROCEDURE Attempt1(parr : REF ARRAY OF PointMetric.T)
  RAISES { Matrix.Singular } =

  PROCEDURE DoFit(READONLY parr : ARRAY OF PointMetric.T)
    RAISES { Matrix.Singular } =
    VAR
      m := NUMBER(parr);

      n := NUMBER(parr[FIRST(parr)].p^);
      
      qdofs  := Qdofs(n);
      ldofs  := Ldofs(n);
      
      xquad  := NEW(REF M.M, m, qdofs);
      xlin   := NEW(REF M.M, m, ldofs);

      ymu    := NEW(REF M.M, m, 1);
      ysigma := NEW(REF M.M, m, 1);
      w      := NEW(REF M.M, m, m);

      ymuHat1,
      ysigmaHat1 : REF M.M;

      ymuHat2,
      ysigmaHat2 : REF M.M;

      rmu1    := NEW(Regression.T);
      rsigma1 := NEW(Regression.T);
      rmu2    := NEW(Regression.T);
      rsigma2 := NEW(Regression.T);

    BEGIN
      M.Zero(w^);
      FOR i := 0 TO m - 1 DO
        WITH rec = parr[i] DO
          ComputeIndepsQ(rec.p, i, xquad^);
          ComputeIndepsL(rec.p, i, xlin^);

          ymu   [i, 0] := MultiEval.Mean   (rec.result);
          ysigma[i, 0] := MultiEval.Sdev   (rec.result);
          
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);
 
      Regression.Run(xlin,
                     ymu   , ymuHat1   , FALSE, rmu1   , 0.0d0, W := w);
      Regression.Run(xlin,
                     ysigma, ysigmaHat1, FALSE, rsigma1, 0.0d0, W := w);
      Regression.Run(xquad,
                     ymu   , ymuHat2   , FALSE, rmu2   , 0.0d0, W := w);
      Regression.Run(xquad,
                     ysigma, ysigmaHat2, FALSE, rsigma2, 0.0d0, W := w);

      WITH rssMu0  = VarM(ymu),
           rssMu1  = Rss(ymu   , ymuHat1),
           rssMu2  = Rss(ymu   , ymuHat2),
           rssSig0 = VarM(ysigma),
           rssSig1 = Rss(ysigma, ysigmaHat1),
           rssSig2 = Rss(ysigma, ysigmaHat2) DO
        Debug.Out(FN("AttemptStatFits mu  : m=%s rss0=%s rss1=%s rss2=%s",
                     TA{Int(m), LR(rssMu0), LR(rssMu1), LR(rssMu2) }));
        Debug.Out(FN("AttemptStatFits sig : m=%s rss0=%s rss1=%s rss2=%s",
                     TA{Int(m), LR(rssSig0), LR(rssSig1), LR(rssSig2) }));
      END
      
    END DoFit;

  VAR
    m := 1;
  BEGIN
    REPEAT
      DoFit(SUBARRAY(parr^, 0, m));
      m := m * 2
    UNTIL m > NUMBER(parr^);
  END Attempt1;

BEGIN END StatFits.

