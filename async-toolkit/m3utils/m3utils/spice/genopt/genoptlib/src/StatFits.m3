MODULE StatFits EXPORTS StatFits, Likelihood;
IMPORT LRMatrix2 AS M;
IMPORT LongrealPQ;
IMPORT MultiEvalLR AS MultiEval;
IMPORT LRRegression AS Regression;
IMPORT LongrealType;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal, FN;
IMPORT LRVector;
IMPORT PointMetricLR;
IMPORT Math;
IMPORT PointMetricLRArraySort AS PointMetricArraySort;
IMPORT Wx;
IMPORT StatFitsSeq;
IMPORT StatFitsArraySort;
IMPORT Matrix;
FROM StatFitsCmp IMPORT CompareByMeanValLikelihood,
                        CompareByMeanAllLikelihood,
                        CompareBySumAbsLinCoeff;
IMPORT ResponseModel;
FROM ResponseModel IMPORT Dofs, Indeps, M2Q, Compute;

FROM GenOptUtils IMPORT FmtP;

FROM SurfaceRep IMPORT FmtQ;
IMPORT StatComponent;

CONST LR = LongReal;
TYPE  TA = ARRAY OF TEXT;

VAR doDebug := Debug.DebugThis("StatFits");
      
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

    Wx.PutText  (wx, "p0     = " & FmtP(t.p0));
    Wx.PutChar(wx, '\n');

    Wx.PutText  (wx, "nomRho = " & LR(t.nomRho));
    Wx.PutChar(wx, '\n');
    
    Wx.PutText  (wx, "orders = ");
    WxOrderNames(wx, t.orders);
    Wx.PutChar(wx, '\n');

    Wx.PutText  (wx, "nom    = " & FmtQ(t.n, t.bnom));
    Wx.PutChar(wx, '\n');

    Wx.PutText  (wx, "mu     = " & FmtQ(t.n, t.bmu));
    Wx.PutChar(wx, '\n');

    Wx.PutText  (wx, "sigma  = " & FmtQ(t.n, t.bsigma));
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

    Wx.PutText  (wx, "radius = " & LR(t.radius));
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

PROCEDURE DoNomFit(READONLY parr : ARRAY OF PointMetricLR.T;
                   type          : ResponseModel.Order;
                   lambdaMult    : LONGREAL) : REF M.M
  RAISES { Matrix.Singular } =

  VAR
    m       := NUMBER(parr);
    n       := NUMBER(parr[FIRST(parr)].p^);
    dofs    := Dofs[type](n);
    x       := NEW(REF M.M, m, dofs);
    ynom    := NEW(REF M.M, m, 1);
    rnom    := NEW(Regression.T);

    ynomHat  : REF M.M;

  BEGIN
    FOR i := 0 TO m - 1 DO
      WITH rec = parr[i] DO
        Indeps[type](rec.p, i, x^);

        (* questionable whether we should use the same number of points
           for nom as for mu, sigma *)
        ynom   [i, 0] := MultiEval.Nominal   (rec.result);
      END
    END;

    Regression.Run(x,
                   ynom   , ynomHat   , FALSE, rnom   ,
                   LambdaFunc(lambdaMult, x));

    RETURN M2Q[type](n, rnom.b^);
  END DoNomFit;

PROCEDURE LambdaFunc(lm : LONGREAL;  m : REF M.M) : LONGREAL =
  BEGIN
    IF lm = 0.0d0 THEN
      RETURN 0.0d0
    ELSE
      RETURN lm * Math.sqrt(M.MeanSqM(m^))
    END
  END LambdaFunc;
  
PROCEDURE Attempt(p                : LRVector.T;
                  parr             : REF ARRAY OF PointMetricLR.T;
                  selectByAll      : BOOLEAN;
                  orders           : ARRAY StatComponent.T OF ResponseModel.Order;
                  nomRho           : LONGREAL;
                  lambdaMult       : LONGREAL) : T
  RAISES { Matrix.Singular, NotEnoughPoints } =

  VAR
    nmOrder := orders[StatComponent.T.Nom];
    muOrder := orders[StatComponent.T.Mu];
    sgOrder := orders[StatComponent.T.Sigma];

  PROCEDURE DoMuSigmaFit(READONLY parr, varr : ARRAY OF PointMetricLR.T)
    RAISES { Matrix.Singular } =

    PROCEDURE DoPoint(READONLY v : PointMetricLR.T; sMu, sSig : LONGREAL) =
      BEGIN
        WITH
          (* data *)
          dmu   = MultiEval.Mean   (v.result), 
          dsig  = MultiEval.Sdev   (v.result),
          
          ns   = v.result.n,
          nsf  = FLOAT(ns, LONGREAL),
          
          (* predicted *)
          pmu  = Compute[muOrder](v.p, rmu.b),
          psig = Compute[sgOrder](v.p, rsigma.b),
          
          (* likelihoods -- weighted by n/nsf *)
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

      xmu     := NEW(REF M.M, m, muDofs);
      xsg     := NEW(REF M.M, m, sgDofs);
      
      ymu     := NEW(REF M.M, m, 1);
      ysigma  := NEW(REF M.M, m, 1);
      w       := NEW(REF M.M, m, m);

      ymuHat,
      ysigmaHat : REF M.M;

      rmu                      := NEW(Regression.T);
      rsigma                   := NEW(Regression.T);

      sumlMu, sumlSig          := 0.0d0;
      sump                     := 0;
      pq                       := NEW(LongrealPQ.Default).init();

      mostDistant              := varr[LAST(varr)].p;
      radius                   := Vdist(p, mostDistant);
      
    BEGIN
      
      M.Zero(w^);
      FOR i := 0 TO m - 1 DO
        WITH rec = parr[i] DO
          Indeps[muOrder](rec.p, i, xmu^);
          Indeps[sgOrder](rec.p, i, xsg^);

          (* questionable whether we should use the same number of points
             for nom as for mu, sigma *)
          ymu   [i, 0] := MultiEval.Mean   (rec.result);
          ysigma[i, 0] := MultiEval.Sdev   (rec.result);
          
          w[i, i]      := FLOAT(rec.result.n, LONGREAL)
        END
      END(*ROF*);

      Regression.Run(xmu,
                     ymu   , ymuHat   , FALSE, rmu   ,
                     LambdaFunc(lambdaMult, xmu),
                     W := w);
      Regression.Run(xsg,
                     ysigma, ysigmaHat, FALSE, rsigma,
                     LambdaFunc(lambdaMult, xmu),
                     W := w);

      WITH rssMu  = Rss(ymu   , ymuHat),
           sMu    = Math.sqrt(rssMu / mf),
           rssSig = Rss(ysigma, ysigmaHat),
           sSig   = Math.sqrt(rssSig / mf),
           debug  = FN("AttemptStatFits2 m=%s radius=%s rssMu=%s rssSig=%s ; sMu=%s sSig=%s",
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

          ll     := sumlMu + sumlSig;
          nllf   := FLOAT(sump, LONGREAL);

          WITH fit =  T { debug,
                          LRVector.Copy(p),
                          thefits.size(),
                          n,
                          l,
                          lmu, lsig,
                          nlf,
                          bnom,
                          M2Q[muOrder](n, rmu.b^),
                          M2Q[sgOrder](n, rsigma.b^),
                          ll,
                          nllf,
                          pts     := pq,
                          evals   := sump,
                          radius  := radius,
                          rank    := ARRAY Ranking OF CARDINAL { LAST(CARDINAL), .. },
                          orders  := orders,
                          nomRho  := nomRho

            } DO
            thefits.addhi(fit)
          END
        END;
      END
    END DoMuSigmaFit;

  PROCEDURE ComparePMByDistance(READONLY a, b : PointMetricLR.T) : [-1 .. 1] =
    BEGIN
      WITH ad = Vdist(a.p, p),
           bd = Vdist(b.p, p) DO
        RETURN LongrealType.Compare(ad, bd)
      END
    END ComparePMByDistance;
    
  CONST
    Comparer = ARRAY Ranking OF CmpProc { CompareByMeanValLikelihood,
                                          CompareByMeanAllLikelihood,
                                          CompareBySumAbsLinCoeff };
  VAR
    m                  := 2;
    max                := NUMBER(parr^) - LeaveOut;

    bestr              := LAST(CARDINAL);
    thefits            := NEW(StatFitsSeq.T).init();

    bnom     : REF M.M := NIL;

    n                  := NUMBER(parr[FIRST(parr^)].p^);
      
    nmDofs             := Dofs[nmOrder](n);
    muDofs             := Dofs[muOrder](n);
    sgDofs             := Dofs[sgOrder](n);
    
    didfit             := FALSE;

    bestfits : T;
    fa       : REF ARRAY OF T;

  BEGIN
    PointMetricArraySort.Sort(parr^, cmp := ComparePMByDistance);

    (* we can immediately do the Nom fit here, parr are sorted in distance
       from p *)

    IF doDebug THEN
      Debug.Out(F("StatFits.Attempt NUMBER(parr^)=%s LeaveOut=%s",
                  Int(NUMBER(parr^)),
                  Int(LeaveOut)))
    END;
              
    IF nomRho # 0.0d0 THEN
      VAR
        lim := NUMBER(parr^);
      BEGIN
        FOR i := FIRST(parr^) TO LAST(parr^) DO
          IF Vdist(parr[i].p, p) > nomRho THEN
            lim := i;
            EXIT
          END
        END;
        IF doDebug THEN
          Debug.Out(F("StatFits.Attempt NOM nomRho=%s , lim=%s",
                      LR(nomRho),
                      Int(lim)))
        END;

        IF lim < nmDofs + LeaveOut THEN
          Debug.Out(F("StatFits.Attempt : lim = %s < nmDofs + LeaveOut = %s",
                      Int(lim), Int(nmDofs + LeaveOut)));
            
          RAISE NotEnoughPoints
        END;
        bnom := DoNomFit(SUBARRAY(parr^, 0, lim), nmOrder, lambdaMult)
      END
    END;
    
    LOOP
      IF doDebug THEN
        Debug.Out(F("StatFits.Attempt NUMBER(parr^)=%s LeaveOut=%s max=%s m=%s",
                    Int(NUMBER(parr^)),
                    Int(LeaveOut),
                    Int(max),
                    Int(m)))
      END;

      WITH lim = MIN(max,m) DO
        IF lim >= muDofs + LeaveOut AND lim >= sgDofs + LeaveOut THEN
          DoMuSigmaFit(SUBARRAY(parr^,   0, lim),
                       SUBARRAY(parr^, lim, LeaveOut));
          didfit := TRUE
        END
      END;
      IF m >= max THEN
        EXIT
      END;
      m := m * 2
    END;

    IF NOT didfit THEN RAISE NotEnoughPoints END;
    
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
        
        IF doDebug THEN Debug.Out(Format(f)) END;
        
        IF sr < bestr THEN
          bestfits := f;
          bestr    := sr
        END
      END
    END;
    
    IF doDebug THEN Debug.Out("BEST RANKED FIT\n" & Format(bestfits)) END;

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

PROCEDURE WxOrderNames(wx : Wx.T; READONLY a : ARRAY OF ResponseModel.Order) =
  BEGIN
    Wx.PutText(wx, "{ ");
    FOR i := FIRST(a) TO LAST(a) DO
      Wx.PutText(wx, ResponseModel.OrderNames[a[i]]);
      Wx.PutChar(wx, ' ')
    END;
    Wx.PutText(wx, "}")
  END WxOrderNames;
  
BEGIN END StatFits.

