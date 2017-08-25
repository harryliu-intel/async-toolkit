(* $Id: ANOVASuper.m3,v 1.3 2010/04/22 08:28:55 mika Exp $ *)

MODULE ANOVASuper;
IMPORT ANOVA, ANOVAClass;
FROM ANOVA IMPORT Order, Mapper, Confidence, FactorValueIterator, EmptyArrC;
FROM ANOVAClass IMPORT RarrL, RarrO, RarrC, 
                       UnitVector, ComponentMul, Normalize;
IMPORT Math, LongrealList, LongrealSort, LongRealSeq;
IMPORT drdist;

REVEAL
  T = ANOVAClass.Private BRANDED Brand OBJECT
    cachedPopDf : [-1..LAST(CARDINAL)] := -1;
    cachedOverallSdev : LONGREAL;
  OVERRIDES
    interactionCoeffs := InteractionCoeffs;

    sdev := Sdev2;
    mean := Mean2;

    allValues := AllValues;

    totalSS := TotalSS2;

    ranked := Ranked2;
    min := Min;
    max := Max;
    median := Median;
    confidence := CalcConfidence;
    popDf := CalcPopDf;
    overallSdev := CalcOverallSdev;
    iterateAllFactorValues := IterateAllFactorValues;
    interactionSS := InteractionSS;
  END;

TYPE
  SdevMapper = Mapper OBJECT
    sum, sumsq, n := 0.0d0;
    df : [-1..LAST(CARDINAL)] := -1;
  OVERRIDES
    map := SdevMap;
  END;

PROCEDURE SdevMap(m : SdevMapper; val : LONGREAL) =
  BEGIN 
    m.sum := m.sum + val; 
    m.sumsq := m.sumsq + val*val; 
    m.n := m.n + 1.0d0; 
    INC(m.df)
  END SdevMap;

PROCEDURE Sdev2(t : T;  READONLY overI, overV : ARRAY OF CARDINAL; VAR df : CARDINAL) : LONGREAL =
  VAR
    m := NEW(SdevMapper);
  BEGIN
    t.mapMatching(overI,overV,m);
    df := m.df;

    RETURN Math.sqrt(m.sumsq/(m.n-1.0d0) - (m.sum/(m.n-1.0d0))*(m.sum/m.n))
  END Sdev2;

PROCEDURE Mean2(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : LONGREAL =
  VAR
    m := NEW(SdevMapper);
  BEGIN
    t.mapMatching(overI,overV,m);
    RETURN m.sum/m.n
  END Mean2;

TYPE 
  SeqMapper = Mapper OBJECT
    seq : LongRealSeq.T;
  OVERRIDES
    map := SeqMap;
  END;

PROCEDURE SeqMap(m : SeqMapper; val : LONGREAL) =
  BEGIN m.seq.addhi(val) END SeqMap;

PROCEDURE AllValues(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : LongRealSeq.T =
  VAR
    m := NEW(SeqMapper, seq := NEW(LongRealSeq.T).init());

  BEGIN
    t.mapMatching(overI,overV,m);
    RETURN m.seq
  END AllValues;

PROCEDURE TotalSS2(t : T) : LONGREAL =
  VAR
    m := NEW(SdevMapper);
  BEGIN
    t.mapMatching(EmptyArrC,EmptyArrC,m);
    RETURN m.sumsq
  END TotalSS2;

TYPE
  RankMapper = Mapper OBJECT
    data : LongrealList.T := NIL;
  OVERRIDES
    map := RankMap;
  END;

PROCEDURE RankMap(m : RankMapper; val : LONGREAL) =
  BEGIN m.data := LongrealList.Cons(val,m.data) END RankMap;

PROCEDURE Ranked2(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : RarrL =
  VAR
    m := NEW(RankMapper);
    res : RarrL;
  BEGIN
    t.mapMatching(overI,overV,m);

    res := NEW(RarrL, LongrealList.Length(m.data));
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := m.data.head;
      m.data := m.data.tail
    END;
    
    LongrealSort.Sort(res^);
    RETURN res
  END Ranked2;

PROCEDURE Min(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : LONGREAL =
  VAR
    r := t.ranked(overI,overV);
  BEGIN
    RETURN r[0]
  END Min;

PROCEDURE Max(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : LONGREAL =
  VAR
    r := t.ranked(overI,overV);
  BEGIN
    RETURN r[LAST(r^)]
  END Max;

PROCEDURE Median(t : T; READONLY overI, overV : ARRAY OF CARDINAL) : LONGREAL =
  VAR
    r := t.ranked(overI,overV);
  BEGIN
    IF NUMBER(r^) MOD 2 = 0 THEN
      RETURN 
        (r[(FIRST(r^)+LAST(r^)) DIV 2] + r[(FIRST(r^)+LAST(r^)) DIV 2+1]) / 2.0d0
    ELSE
      RETURN r[(FIRST(r^)+LAST(r^)) DIV 2]
    END
  END Median;


(* from anova/program/src/Main.m3... ugly *)

PROCEDURE CalcPopDf(t : T) : CARDINAL = 
  BEGIN
    IF t.cachedPopDf = -1 THEN 
      VAR 
        c : CARDINAL;
      BEGIN
        t.cachedOverallSdev := 
            t.sdev(ARRAY OF CARDINAL{}, ARRAY OF CARDINAL{}, c);
        t.cachedPopDf := c
      END
    END;
    RETURN t.cachedPopDf 
  END CalcPopDf;

PROCEDURE CalcOverallSdev(t : T) : LONGREAL = 
  BEGIN
    IF t.cachedPopDf = -1 THEN 
      VAR 
        c : CARDINAL;
      BEGIN
        t.cachedOverallSdev := 
            t.sdev(ARRAY OF CARDINAL{}, ARRAY OF CARDINAL{}, c);
        t.cachedPopDf := c
      END
    END;
    RETURN t.cachedOverallSdev
  END CalcOverallSdev;

PROCEDURE CalcConfidence(t : T; READONLY overI, overV : ARRAY OF CARDINAL; 
                     level : LONGREAL) : Confidence =
  VAR
    df : CARDINAL;
    mean := t.mean(overI,overV);
    sdev := t.sdev(overI,overV,df);
    sampM1 : INTEGER := df;
    nM1 : INTEGER := t.popDf();
    oneMlevel := 1.0d0-level;

    ls   := drdist.Studin(sampM1,level);
    us   := drdist.Studin(sampM1,oneMlevel);
    lp   := drdist.Studin(nM1,level);
    up   := drdist.Studin(nM1,oneMlevel);
    
    lsc := mean + ls*sdev;
    lpc := mean + lp*t.overallSdev();
    upc := mean + up*t.overallSdev();
    usc := mean + us*sdev;
  BEGIN
    RETURN
      Confidence { mean := mean,
                   lower := lsc,
                   lowerPooled := lpc,
                   upperPooled := upc,
                   upper := usc }
  END CalcConfidence;


PROCEDURE IterateAllFactorValues(t : T;
                                 READONLY factors : ARRAY OF CARDINAL;
                                 iter : FactorValueIterator) =
  PROCEDURE Recurse(READONLY vals : ARRAY OF CARDINAL) =
    BEGIN
      IF NUMBER(vals) = NUMBER(factors) THEN
        iter.callback(factors,vals)
      ELSE

        VAR
          newVals := NEW(RarrC, NUMBER(vals) + 1);
        BEGIN
          SUBARRAY(newVals^,0,NUMBER(vals)) := vals;

          FOR j := 0 TO t.levels(factors[LAST(newVals^)])-1 DO
            newVals[LAST(newVals^)] := j;
            Recurse(newVals^)
          END
        END
      END
    END Recurse;

  CONST
    rootvals = ARRAY OF CARDINAL {};
  BEGIN
    Recurse(rootvals)
  END IterateAllFactorValues;

PROCEDURE InteractionSS(t : T; READONLY orders : ARRAY OF Order) : LONGREAL=
  VAR
    o := NEW(RarrO, NUMBER(orders));
  BEGIN
    FOR i := FIRST(orders) TO LAST(orders) DO
      o[i] := orders[i]
    END;

    (* -1 case... recurse *)
    FOR i := FIRST(orders) TO LAST(orders) DO
      IF orders[i] = -1 THEN
        VAR
          res := 0.0d0;
        BEGIN
          FOR j := 1 TO t.levels(i)-1 DO
            o[i] := j;
            res := res + t.interactionSS(o^)
          END;
          RETURN res
        END
      END
    END;

    VAR
      resp := t.interactionEffect(o^);
    BEGIN
      RETURN resp * resp
    END
   END InteractionSS;

PROCEDURE InteractionCoeffs(t : T; READONLY orders : ARRAY OF Order) : RarrL =
  VAR
    coeffs := UnitVector(t.numValues());
  BEGIN
    FOR i := FIRST(orders) TO LAST(orders) DO
      ComponentMul(t.mainEffCoeffs(i,orders[i])^,
                   coeffs^,
                   coeffs^)
    END;
    Normalize(coeffs^);
    RETURN coeffs
  END InteractionCoeffs;
  
BEGIN END ANOVASuper.

