MODULE Main;
IMPORT ParseParams;
IMPORT Debug;
IMPORT LongrealArraySort AS LRSort;
IMPORT LongRealSeq AS LRSeq;
IMPORT Math;
IMPORT Fmt; FROM Fmt IMPORT F, Int, Bool;
IMPORT Text;
IMPORT OSError, Wr, Thread;
IMPORT Pathname;
IMPORT Rd, FileRd;
IMPORT Stdio;
IMPORT FloatMode, Lex;
IMPORT Random;
IMPORT NormalDeviate AS Normal;
IMPORT AL;
IMPORT Histogram;
IMPORT LRVector, LRScalarField;
IMPORT RefSeq;
IMPORT NewUOAs;
IMPORT NewUOA_M3;
IMPORT Wx;
IMPORT FileWr;
IMPORT ReadDist;
IMPORT DieData; FROM DieData IMPORT Vids;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E*>

TYPE
  Array = ARRAY OF LONGREAL;
  
CONST LR = Fmt.LongReal;
      TE = Text.Equal;

VAR
  doDebug := TRUE;

TYPE
  Model = OBJECT
    rand       : Random.T;
    vidBuckets : Vids;
  METHODS
    make() : DieData.T;
  END;

  OneRegrModel = Model OBJECT
    sigmaShift : LONGREAL;
    regrCoeffs : ARRAY [0..1] OF LONGREAL;
    margCoeffs : ARRAY [0..1] OF LONGREAL;
  OVERRIDES
    make := MakeORM;
  END;

PROCEDURE MakeORM(model : OneRegrModel) : DieData.T =
  BEGIN
  END MakeORM;
  
PROCEDURE Square(x : LONGREAL) : LONGREAL = BEGIN RETURN x * x END Square;

PROCEDURE RoundUp(x                : LONGREAL;
                  READONLY buckets : Array;
                  VAR res          : LONGREAL;
                  VAR bin          : CARDINAL) : BOOLEAN =
  BEGIN
    FOR i := FIRST(buckets) TO LAST(buckets) DO
      IF x <= buckets[i] THEN
        res := buckets[i];
        bin := i;
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END RoundUp;

PROCEDURE CopyPoly(READONLY poly : Array) : REF Array =
  VAR
    res := NEW(REF Array, NUMBER(poly));
  BEGIN
    res^ := poly;
    RETURN res
  END CopyPoly;

VAR Samples := 1000;
  
PROCEDURE Make(rand                : Random.T;
               READONLY vidBuckets : Array) : DieData.T =
  VAR
    trunc := 3.0d0;
    x, y  := Normal.Trunc(rand, 0.0d0, 1.0d0, -trunc, +trunc);
    res   := NEW(DieData.T, x := x, y := y);
    
  BEGIN
    MakeD(vidBuckets, res);
    RETURN res
  END Make;

PROCEDURE DoVid(VAR res : DieData.T; READONLY vidBuckets : Vids) =
  VAR
    vid   : LONGREAL;
    bin   : CARDINAL;
    haveVid := RoundUp(res.vcust, vidBuckets, vid, bin);
  BEGIN
    IF haveVid THEN
      WITH  rvidP = Square(vid / res.vmin),
            vidP  = res.vminP * rvidP DO
        
        res.vidP := vidP;
        res.vidbin := bin
      END
    ELSE
      res.vidP := LAST(LONGREAL);
      res.vidbin := -1
    END
  END DoVid;

PROCEDURE NearestHigher(step, x : LONGREAL) : LONGREAL =
  BEGIN
    WITH q = CEILING(x / step) DO
      RETURN FLOAT(q, LONGREAL) * step
    END
  END NearestHigher;
  
PROCEDURE MakeD(READONLY vidBuckets : Vids;
                VAR res             : DieData.T) =
  CONST
    Rounding = 0.001d0;
    
  BEGIN
    (* first choose the Vmin *)
    WITH vmin    = Poly(res.x + sigmaShift  ,
                        Array { mean, sdev }),  (* vmin sample *)
         linP    = Poly(vmin, Tf2PowerCoeffs),  (* linear power est *)
         resP    = Poly(res.y   , Array { 0.0d0, residual }),
         vminP   = resP + linP,

         vage    = vmin + marginCoeffs[0],
         rageP   = Square(vage / vmin),
         ageP    = vminP * rageP,
         
         vcust   = vmin + Poly(vmin, marginCoeffs^),
         rcusP   = Square(vcust / vmin),
         custP   = vminP * rcusP,

         vroun   = NearestHigher(Rounding, vcust),
         rrouP   = Square(vroun / vmin),
         rounP   = vminP * rrouP
     DO

      res.vmin  := vmin;
      res.vminP := vminP;
      res.ageP  := ageP;
      res.vcust := vcust;
      res.custP := custP;
      res.vroun := vroun;
      res.rounP := rounP;
      
      DoVid(res, vidBuckets);

      IF FALSE AND doDebug THEN
        Debug.Out(
            F("x=%s y=%s vmin=%s linP=%s resP=%s ",
              LR(res.x), LR(res.y), LR(vmin), LR(linP), LR(resP)) &
            F("vminP=%s vage=%s ageP=%s vcust=%s ",
              LR(vminP), LR(vage), LR(ageP), LR(vcust)) &
            F("custP=%s haveVid=%s vidbin=%s vidP=%s ",
              LR(custP), Bool(res.vidP # LAST(LONGREAL)), Int(res.vidbin), LR(res.vidP)) )
        
      END
    END
  END MakeD;

PROCEDURE Poly(x               : LONGREAL;
               READONLY coeffs : ARRAY OF LONGREAL) : LONGREAL =
  (* polynomial evaluate! *)
  VAR
    y := 0.0d0;
    p := 1.0d0;
  BEGIN
    FOR i := FIRST(coeffs) TO LAST(coeffs) DO
      y := y + coeffs[i] * p;
      p := p * x
    END;
    RETURN y
  END Poly;

TYPE
  YieldEvaluator = LRScalarField.Default OBJECT
    rand               : Random.T;
    cutoffP            : LONGREAL;
    nsamples, batchsiz : CARDINAL;
    aveWeight          : LONGREAL;
    yieldWeight        : LONGREAL;
    binWeight          : LONGREAL;

    vids               : Vids;          (* scratch *)
    samples            : RefSeq.T;      (* state *)
    k                  : CARDINAL := 0; (* pointer to next elem *)

  OVERRIDES
    eval     := EvalYield;
    evalHint := EvalHint;
  END;
    
<*NOWARN*>PROCEDURE EvalHint(ye : YieldEvaluator; cutoffs : LRVector.T) =
  BEGIN
    EVAL EvalYield(ye, cutoffs)
  END EvalHint;

PROCEDURE FmtBins(vids : Vids) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(Vids) TO LAST(Vids) DO
      Wx.PutText(wx, F("%s ", LR(vids[i])))
    END;
    RETURN Wx.ToText(wx)
  END FmtBins;
  
PROCEDURE FmtState(state : State) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(State) TO LAST(State) DO
      Wx.PutText(wx, F("%s ", LR(state[i])))
    END;
    RETURN Wx.ToText(wx)
  END FmtState;
  
PROCEDURE EvalYield(ye : YieldEvaluator; state : LRVector.T) : LONGREAL =
  VAR
    val : LONGREAL;
  BEGIN
    (* copy vids *)
    ye.vids := State2Vids(state^);

    Debug.Out(F("EvalYield with state %s bin limits=%s",
                FmtState(state^),
                FmtBins(ye.vids)));
    
    (* initialization *)
    WHILE ye.samples.size() # ye.nsamples DO
      ye.samples.addhi(Make(ye.rand, ye.vids))
    END;

    FOR i := 0 TO ye.nsamples - 1 DO
      VAR
        s : DieData.T := ye.samples.get(i); (* pointer *)
      BEGIN
        DoVid(s, ye.vids)
      END
    END;

    VAR
      binsDefined  := 0;
      binnedCnt    := 0;
      roundedCnt   := 0;
      sumP         := 0.0d0;
      maxbin       := -1;
      maxvcust     := 0.0d0;
      minvcust     := LAST(LONGREAL);
    BEGIN
      FOR i := 0 TO ye.nsamples - 1 DO
        WITH d = NARROW(ye.samples.get(i), DieData.T) DO
          IF FALSE THEN
            Debug.Out(F("i=%s d.vidP=%s ye.cutoffP=%s",
                        Int(i), LR(d.vidP), LR(ye.cutoffP)))
          END;
          IF d.vidP # LAST(LONGREAL) THEN
            INC(binsDefined)
          END;
          IF d.vidP <= ye.cutoffP THEN
            INC(binnedCnt);
            sumP     := sumP + d.vidP;
            maxbin   := MAX(maxbin, d.vidbin);
            maxvcust := MAX(maxvcust, d.vcust);
            minvcust := MIN(minvcust, d.vcust)
          END;
          IF d.rounP <= ye.cutoffP THEN
            INC(roundedCnt)
          END
        END
      END;
      
      WITH nf           = FLOAT(ye.nsamples, LONGREAL),
           definedProp  = FLOAT(binsDefined, LONGREAL) / nf,
           bcf          = FLOAT(binnedCnt, LONGREAL),

           binnedYield  = bcf / nf,
           aveP         = DivOr(sumP, bcf, 10000.0d0),
           yieldloss    = 1.0d0 - binnedYield,
           yieldlossP   = yieldloss * aveP,
           score        = ye.yieldWeight * -binnedYield +
                          ye.aveWeight * aveP     +
                          ye.binWeight * -FLOAT(maxbin,LONGREAL),

           roundedYield = FLOAT(roundedCnt, LONGREAL) / nf 
       DO
        Debug.Out(F("roundedYield=%s definedProp=%s ",
                    LR(roundedYield),
                    LR(definedProp)) &
                  F("binnedYield=%s yieldloss=%s aveP=%s score=%s maxbin=%s",
                    LR(binnedYield),
                    LR(yieldloss),
                    LR(aveP),
                    LR(score),
                    Int(maxbin)) &
                  F(" minvcust=%s maxvcust=%s", LR(minvcust), LR(maxvcust)));
        RETURN score
      END
    END
  END EvalYield;

PROCEDURE DivOr(n, d, x : LONGREAL) : LONGREAL =
  BEGIN
    IF d = 0.0d0 THEN RETURN x ELSE RETURN n / d END
  END DivOr;
  
VAR
  vminFileN : Pathname.T := NIL;
  mean, sdev : LONGREAL;
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  Unit := 0.001d0; (* file input is in millivolts *)
  cutoffs := NEW(LRSeq.T).init();
  n : CARDINAL;

PROCEDURE AdjustBins(READONLY preVrAdj : Vids) : Vids =
  (* adjust cutoffs from pre-VR adjustment to post-VR adjustment.

     cutoffs are actually applied on the binning, and then customer
     inaccuracy is the 1-st order term of the VR error 

     this allows us to run sims on the cutoffs as the last adjustment
     in the chain rather than recomputing several steps 
  *)
  VAR
    res : Vids;
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := (1.0d0 + marginCoeffs[1]) * preVrAdj[i];
    END;
    RETURN res
  END AdjustBins;

PROCEDURE UnadjustBins(READONLY postVrAdj : Vids) : Vids =
  (* adjust cutoffs from post-VR adjustment to pre-VR adjustment.

     cutoffs are actually applied on the binning, and then customer
     inaccuracy is the 1-st order term of the VR error 

     this allows us to run sims on the cutoffs as the last adjustment
     in the chain rather than recomputing several steps 
  *)
  VAR
    res : Vids;
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      res[i] := 1.0d0 / (1.0d0 + marginCoeffs[1]) * postVrAdj[i];
    END;
    RETURN res
  END UnadjustBins;

TYPE State = ARRAY [0..1] OF LONGREAL;
     
PROCEDURE State2Vids(READONLY state : State) : Vids =
  VAR
    res : Vids;
  BEGIN
    (* convert from optimizer state variables to actual VID voltages *)
    res[0] := state[0];
    WITH delta = Square(state[1]) / FLOAT(NUMBER(Vids) - 1, LONGREAL) DO
      FOR i := 1 TO LAST(res) DO
        res[i] := res[0] + FLOAT(i, LONGREAL) * delta
      END;
      RETURN res
    END
  END State2Vids;
  
PROCEDURE Vids2State(READONLY vids : Vids) : State =
  VAR
    res : State;
  BEGIN
    (* convert from actual VID voltages to optimizer state variables *)
    res[0] := vids[0];
    res[1] := Math.sqrt(vids[LAST(vids)] - vids[0]);
    RETURN res
  END Vids2State;

PROCEDURE DoHistograms(rand             : Random.T;
                       cutoffP          : LONGREAL;
                       READONLY buckets : Vids) =
  VAR
    samples := NEW(REF ARRAY OF DieData.T, Samples);
    hist    := NEW(REF Array, Samples);
    vidCases := 0;
    sfx     := Int(ROUND(cutoffP));

  BEGIN
    Debug.Out(F("DoHistograms cutoffP=%s bin limits=%s",
                LR(cutoffP),
                FmtBins(buckets)));


    FOR i := 0 TO Samples - 1 DO
      samples[i] := Make(rand, buckets);
    END;

    (**********************************************************************)
    
    FOR i := 0 TO Samples - 1 DO
      hist[i]       := samples[i].vminP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("vminP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)
    
    FOR i := 0 TO Samples - 1 DO
      hist[i]       := samples[i].ageP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("ageP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)

    FOR i := 0 TO Samples - 1 DO
      hist[i]       := samples[i].custP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("custP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)

    FOR i := 0 TO Samples - 1 DO
      hist[i]       := samples[i].rounP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("rounP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)

    FOR i := 0 TO Samples - 1 DO
      hist[i]       := samples[i].vidP;
      IF hist[i] # LAST(LONGREAL) THEN
        INC(vidCases)
      END
    END;

    Debug.Out(F("Samples=%s vidCases=%s", Int(Samples), Int(vidCases)));
    
    LRSort.Sort(hist^);
    Histogram.Do("vidP_"&sfx, SUBARRAY(hist^, 0, vidCases) , TRUE, 100);

    WITH wr = FileWr.Open("vminVidP.dat") DO
      FOR i := 0 TO MIN(Samples - 1, 100000) DO
        WITH s = samples[i] DO
          IF s.vidbin # -1 THEN
            Wr.PutText(wr, F("%s %s\n", LR(buckets[s.vidbin]), LR(s.vidP)))
          END
        END
      END;
      Wr.Close(wr)
    END
  END DoHistograms;
  
VAR
  residual := Tf2PowerResidual;

  marginCoeffs := CopyPoly(Tf2MarginCoeffs);

  sigmaShift := 0.0d0;
  
CONST
  Tf2PowerResidual = 9.103652d0;
  (* watts of deviation above and beyond @ ATE V *)

  Tf2PowerCoeffs   = ARRAY OF LONGREAL { 0.3367d0, 466.78d0 };
  (* 
     from Excel -- this is basic power dependence as function of Vmin 
     assuming 105C TDP 

     we are here assuming power is measured AT our ATE Vmin.
  *)
  Tf2MarginCoeffs  = ARRAY OF LONGREAL { 0.030d0, 0.04d0 };
  (* margin is 30 mV + 4% *)

  (* # of VID steps allowed *)

  DineshVmins = Vids { 0.620d0,
                       0.650d0,
                       0.675d0,
                       0.690d0,
                       0.705d0,
                       0.725d0,
                       0.750d0,
                       0.780d0 };

  TestVminsO = Vids { 0.680d0,
                     0.690d0,
                     0.700d0,
                     0.710d0,
                     0.720d0,
                     0.730d0,
                     0.740d0,
                     0.760d0 };

  TestVmins = Vids { 0.670d0,
                     0.671d0,
                     0.672d0,
                     0.673d0,
                     0.674d0,
                     0.675d0,
                     0.676d0,
                     0.700d0 };
BEGIN
  
  TRY
    IF pp.keywordPresent("-f") THEN
      vminFileN := pp.getNext();
    END;

    WHILE pp.keywordPresent("-cutoff") DO
      cutoffs.addhi(pp.getNextLongReal(min := 0.0d0))
    END;

    IF pp.keywordPresent("-residual") THEN
      residual := pp.getNextLongReal(min := 0.0d0)
    END;

    IF pp.keywordPresent("-deltav") THEN
      marginCoeffs[0] := marginCoeffs[0] + pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-S") OR pp.keywordPresent("-samples") THEN
      Samples := pp.getNextInt(min := 1)
    END;

    IF pp.keywordPresent("-sigmashift") THEN
      sigmaShift := pp.getNextLongReal()
    END;
    
    pp.skipParsed()

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;


  IF vminFileN # NIL THEN
    VAR
      title : TEXT;
      rd : Rd.T;
    BEGIN
      TRY
        rd := FileRd.Open(vminFileN);
        ReadDist.Read(rd, Unit, n, mean, sdev, title);
        Rd.Close(rd)
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("error opening input file \"%s\" : OSError.E : %s",
                      vminFileN,
                      AL.Format(x)))
      |
        Rd.Failure(x) =>
        Debug.Error(F("error reading input file \"%s\" : Rd.Failure : %s",
                      vminFileN,
                      AL.Format(x)))
      |
        FloatMode.Trap, Lex.Error =>
        Debug.Error(F("parse error reading input file \"%s\"", vminFileN))
      END;
      
      Debug.Out(F("data read title %s : n %s mean %s sdev %s\n",
                  title,
                  Int(n),
                  LR(mean),
                  LR(sdev)))
    END
  END;

  VAR
    rand := NEW(Random.Default).init();
    
    buckets := AdjustBins(DineshVmins);

    vidBins : Vids;
    f : LONGREAL;
  BEGIN
    FOR coi := 0 TO cutoffs.size() - 1 DO
      WITH ye  = NEW(YieldEvaluator,
                     rand        := rand,
                     cutoffP     := cutoffs.get(coi),
                     nsamples    := Samples,
                     batchsiz    := Samples DIV 100,
                     aveWeight   := 10.0d0,
                     yieldWeight := 1000000.0d0,
                     binWeight   := 0.0d0,
                     
                     samples     := NEW(RefSeq.T).init()),
           vec = NEW(REF ARRAY OF LONGREAL, NUMBER(State)) DO
        vec^ := Vids2State(buckets);
        Debug.Out(F("Starting with cutoff %s : bins %s ; state %s",
                    LR(ye.cutoffP),
                    FmtBins(buckets),
                    FmtState(vec^)));
        IF FALSE THEN
          WITH out = NewUOAs.Minimize(vec,
                                      ye,
                                      rhobeg := 0.01d0,
                                      rhoend := 0.00000001d0) DO
            vidBins := UnadjustBins(State2Vids(out.x^));
            f := out.f;
          END
        ELSE
          WITH ff = NewUOA_M3.Minimize(vec,
                                       ye,
                                       npt := 6,
                                       rhobeg := 0.01d0,
                                       rhoend := 0.0000001d0,
                                       maxfun := 100000) DO
            vidBins := UnadjustBins(State2Vids(vec^));
            f := ff
          END
        END;
        Debug.Out(F("At power cutoff %s : best binning voltages : %s ; score %s",
                    LR(cutoffs.get(coi)),
                    FmtBins(vidBins),
                    LR(f)));
        
        DoHistograms(rand, cutoffs.get(coi), AdjustBins(vidBins))
      END
    END

  END
END Main.
