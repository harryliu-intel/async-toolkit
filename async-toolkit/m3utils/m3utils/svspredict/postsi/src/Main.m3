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
IMPORT Scan;
IMPORT Stdio;
IMPORT FloatMode, Lex;
IMPORT Random;
IMPORT NormalDeviate AS Normal;
IMPORT Sortable;
IMPORT AL;
IMPORT Histogram;
IMPORT LRVector, LRScalarField;
IMPORT RefSeq;
IMPORT NewUOAs;
IMPORT NewUOA_M3;
IMPORT Wx;
IMPORT FileWr;

<*FATAL Thread.Alerted, Wr.Failure, OSError.E*>

TYPE
  Array = ARRAY OF LONGREAL;
  
CONST LR = Fmt.LongReal;
      TE = Text.Equal;

VAR
  doDebug := TRUE;

TYPE
  DieData = Sortable.T OBJECT
    x, y  : LONGREAL;   (* dist. #s *)

    vmin  : LONGREAL;   (* ATE Vmin *)
    vminP : LONGREAL;   (* P @ ATE Vmin *)

    ageP  : LONGREAL;   (* P @ EOL (age margin + ATE margin) *)

    vcust : LONGREAL;   (* customer V (incl. PS margin) *)
    custP : LONGREAL;   (* P @ vcust *)

    vidbin: [-1 .. LAST(Vids) ] ;   (* which VID bin *)
    
    vidP  : LONGREAL;   (* P @ VID (incl. VID round-off) *)
  END;

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
               READONLY vidBuckets : Array) : DieData =
  VAR
    trunc := 3.0d0;
    x, y  := Normal.Trunc(rand, 0.0d0, 1.0d0, -trunc, +trunc);
    res := NEW(DieData, x := x, y := y);
    
  BEGIN
    MakeD(vidBuckets, res);
    RETURN res
  END Make;

PROCEDURE DoVid(VAR res : DieData; READONLY vidBuckets : Vids) =
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

PROCEDURE MakeD(READONLY vidBuckets : Array;
                VAR res             : DieData) =
  BEGIN
    (* first choose the Vmin *)
    WITH vmin    = Poly(res.x   , Array { mean, sdev }),  (* vmin sample *)
         linP    = Poly(vmin, Tf2PowerCoeffs),        (* linear power est *)
         resP    = Poly(res.y   , Array { 0.0d0, residual }),
         vminP   = resP + linP,

         vage    = vmin + marginCoeffs[0],
         rageP   = Square(vage/vmin),
         ageP    = vminP * rageP,
         
         vcust   = vmin + Poly(vmin, marginCoeffs^),
         rcusP   = Square(vcust/vmin),
         custP   = vminP * rcusP DO

      res.vmin  := vmin;
      res.vminP := vminP;
      res.ageP  := ageP;
      res.vcust := vcust;
      res.custP := custP;

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
    eval := EvalYield;
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
    WHILE ye.samples.size() < ye.nsamples DO
      ye.samples.addhi(Make(ye.rand, ye.vids))
    END;

    IF FALSE THEN
      (* replace first batch *)
      FOR i := 0 TO ye.batchsiz - 1 DO
        ye.samples.put(ye.k, Make(ye.rand, ye.vids));
        ye.k := (ye.k + 1) MOD ye.nsamples
      END
    END;

    FOR i := 0 TO ye.nsamples - 1 DO
      VAR
        s : DieData := ye.samples.get(i);
      BEGIN
        DoVid(s, ye.vids)
      END
    END;

    VAR
      cnt    := 0;
      sumP   := 0.0d0;
      maxbin := -1;
      maxvcust := 0.0d0;
      minvcust := LAST(LONGREAL);
    BEGIN
      FOR i := 0 TO ye.nsamples - 1 DO
        WITH d = NARROW(ye.samples.get(i), DieData) DO
          IF FALSE THEN
            Debug.Out(F("i=%s d.vidP=%s ye.cutoffP=%s",
                        Int(i), LR(d.vidP), LR(ye.cutoffP)))
          END;
          IF d.vidP <= ye.cutoffP THEN
            INC(cnt);
            sumP := sumP + d.vidP;
            maxbin := MAX(maxbin, d.vidbin);
            maxvcust := MAX(maxvcust, d.vcust);
            minvcust := MIN(minvcust, d.vcust);
          END
        END
      END;
      
      WITH nf         = FLOAT(ye.nsamples, LONGREAL),
           cf         = FLOAT(cnt, LONGREAL),
           yield      = cf / nf,
           aveP       = DivOr(sumP, cf, 10000.0d0),
           yieldloss  = 1.0d0 - yield,
           yieldlossP = yieldloss * aveP,
           score      = ye.yieldWeight * -yield +
                        ye.aveWeight * aveP     +
                        ye.binWeight * -FLOAT(maxbin,LONGREAL)
       DO
        Debug.Out(F("yield=%s yieldloss=%s aveP=%s score=%s maxbin=%s",
                    LR(yield),
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
  seq := NEW(LRSeq.T).init();
  arr : REF ARRAY OF LONGREAL;
  mean, var, sdev : LONGREAL;
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  Unit := 0.001d0; (* file input is in millivolts *)
  cutoffs := NEW(LRSeq.T).init();

TYPE
  Vids = ARRAY [0..Tf2VidSteps-1] OF LONGREAL;

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
    samples := NEW(REF ARRAY OF Sortable.T, Samples);
    hist    := NEW(REF Array, Samples);
    vidCases := 0;
    sfx     := Int(ROUND(cutoffP));

  BEGIN

    FOR i := 0 TO Samples - 1 DO
      samples[i] := Make(rand, buckets);
    END;

    (**********************************************************************)
    
    FOR i := 0 TO Samples - 1 DO
      hist[i]       := NARROW(samples[i], DieData).vminP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("vminP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)
    
    FOR i := 0 TO Samples - 1 DO
      hist[i]       := NARROW(samples[i], DieData).ageP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("ageP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)

    FOR i := 0 TO Samples - 1 DO
      hist[i]       := NARROW(samples[i], DieData).custP
    END;
    LRSort.Sort(hist^);
    Histogram.Do("custP_"&sfx, hist^, TRUE, 100);

    (**********************************************************************)

    FOR i := 0 TO Samples - 1 DO
      hist[i]       := NARROW(samples[i], DieData).vidP;
      IF hist[i] # LAST(LONGREAL) THEN
        INC(vidCases)
      END
    END;

    Debug.Out(F("Samples=%s vidCases=%s", Int(Samples), Int(vidCases)));
    
    LRSort.Sort(hist^);
    Histogram.Do("vidP_"&sfx, SUBARRAY(hist^, 0, vidCases) , TRUE, 100);

    WITH wr = FileWr.Open("vminVidP.dat") DO
      FOR i := 0 TO Samples - 1 DO
        WITH s = NARROW(samples[i], DieData) DO
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

  Tf2VidSteps = 8;
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
    
    pp.skipParsed()

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;


  IF vminFileN # NIL THEN
    TRY
      VAR
        rd := FileRd.Open(vminFileN);
        line : TEXT;
        val : LONGREAL;
        lno := 0;
      BEGIN
        INC(lno);
        EVAL Rd.GetLine(rd);
        LOOP
          INC(lno);
          line := Rd.GetLine(rd);
          val := Scan.LongReal(line) * Unit;
          seq.addhi(val);
        END;

      END
    EXCEPT
      Rd.EndOfFile => (* ok *)
    |
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
    END
  END;

  arr := NEW(REF ARRAY OF LONGREAL, seq.size());
  FOR i := 0 TO seq.size() - 1 DO
    arr[i] := seq.get(i)
  END;

  LRSort.Sort(arr^); (* not needed, eh? *)

  VAR
    sumsq, sum := 0.00d0;
    nf         := FLOAT(NUMBER(arr^),LONGREAL);
    corr       := Math.sqrt(nf / ( nf - 1.0d0 ));
  BEGIN
    FOR i := 0 TO NUMBER(arr^) - 1 DO
      sumsq := sumsq + arr[i] * arr[i];
      sum   := sum + arr[i]
    END;
    mean := sum / nf;
    var  := corr * ( sumsq / nf - (mean * mean) );
    sdev := Math.sqrt(var);
  END;

  Debug.Out(F("n %s mean %s sdev %s\n", Int(NUMBER(arr^)), LR(mean), LR(sdev)));

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
        
        DoHistograms(rand, cutoffs.get(coi), vidBins)
      END
    END

  END
END Main.
