(* $Id: ANOVAProgramBits.m3,v 1.3 2010/04/22 08:28:55 mika Exp $ *)

MODULE ANOVAProgramBits;

IMPORT Debug;
IMPORT IO;
IMPORT FactorialDatumList, FactorialDatum;
IMPORT ANOVA, Fmt, drdist;
FROM Fmt IMPORT Style;
IMPORT Text, IntPQ;
IMPORT Math;
IMPORT LongrealPQ;
IMPORT CardSubarrays;

REVEAL
  F = PubF BRANDED OBJECT
  OVERRIDES
    resultCallback := Callback;
  END;

TYPE
  RarrC = REF ARRAY OF CARDINAL;
  ArrC  = ARRAY OF CARDINAL;
  RarrL = REF ARRAY OF LONGREAL;
  ArrL  = ARRAY OF LONGREAL;
  ArrT  = ARRAY OF TEXT;

PROCEDURE Power(p : CARDINAL) : TEXT =
  BEGIN
    CASE p OF
      0 => RETURN "zeroth"
    | 1 => RETURN "linear"
    | 2 => RETURN "quadratic"
    | 3 => RETURN "cubic"
    | 4 => RETURN "quartic"
    | 5 => RETURN "quintic"
    | 6 => RETURN "sextic"
    | 7 => RETURN "septic"
    | 8 => RETURN "octic"
    ELSE
      VAR
        suffix := "th";
      BEGIN
        IF p MOD 100 < 10 OR p MOD 100 >= 20 THEN
          CASE p MOD 10 OF
            1 => suffix := "st"
          | 2 => suffix := "nd"
          | 3 => suffix := "rd"
          ELSE
          END
        END;
        RETURN Fmt.Int(p) & suffix
      END
    END
  END Power;

PROCEDURE RemQuotes(tx : TEXT) : TEXT =
  VAR
    l := Text.Length(tx);
    arr := NEW(REF ARRAY OF CHAR, l);
    p := 0;
  BEGIN
    FOR i := 0 TO l - 1 DO
      WITH c = Text.GetChar(tx,i) DO
        IF c # '"' THEN arr[p] := c; INC(p) END
      END
    END;
    RETURN Text.FromChars(SUBARRAY(arr^,0,p))
  END RemQuotes;

PROCEDURE Callback(t : F; 
                   READONLY vi : ARRAY OF CARDINAL;
                   READONLY r  : ARRAY OF LONGREAL;
                   src : TEXT) =
   VAR
     vip := NEW(RarrC, NUMBER(vi));
     rp := NEW(REF ARRAY OF LONGREAL, NUMBER(r));
   BEGIN
     vip^ := vi;
     rp^ := r;
     t.list := FactorialDatumList.Cons(FactorialDatum.T { vip,rp,src }, t.list)
   END Callback;

TYPE FM = { Full, NoRes, Error, Total };


CONST
  Prec = 4;
  PadWidth = Prec + 10;
  FirstWidth = 30;
  
PROCEDURE Line(c : CHAR) =
  CONST
    Cols = 7;
  BEGIN
    IO.Put(Fmt.Pad("", 
                   length := PadWidth*(Cols-1)+3 + FirstWidth, 
                   padChar := c));
    IO.Put("\n")
  END Line;
  
VAR noround := FALSE;

PROCEDURE SetNoRound() = BEGIN noround := TRUE END SetNoRound;
(* ugly *)

PROCEDURE Num(l : LONGREAL; 
              style := Style.Fix; 
              hiPrec := FALSE; 
              suppressIfZero := FALSE
              ) : TEXT =
  VAR
    prec := Prec;
  BEGIN
    IF noround THEN
      RETURN Fmt.Pad(Fmt.LongReal(l),
                     length := PadWidth)
    END;
    IF l # 0.0d0 AND ABS(l) < 1.0d-3 OR ABS(l) > 1.0d5 THEN 
      style := Style.Sci 
    ELSE
      IF hiPrec THEN prec := 4 END;
    END;
    IF suppressIfZero AND l = 0.0d0 THEN
      RETURN Fmt.Pad("",length := PadWidth)
    ELSE
      RETURN Fmt.Pad(Fmt.LongReal(l, prec := prec, style := style), 
                     length := PadWidth)
    END
  END Num;
  
PROCEDURE Int(i : INTEGER) : TEXT =
  BEGIN
    RETURN Fmt.Pad(Fmt.Int(i),length := PadWidth)
  END Int;

PROCEDURE TX(t : TEXT; width := -1; align := Fmt.Align.Right) : TEXT =
  VAR
    l := PadWidth;
  BEGIN
    IF width # -1 THEN l := width END;
    RETURN Fmt.Pad(t, length := l, align := align)
  END TX;

PROCEDURE Title() = 
  BEGIN
    IO.Put(TX(" Source", width := FirstWidth, align := Fmt.Align.Left) & TX("Response")& TX("SS") &
                                      TX("df") & 
                                      TX("MSS") & TX("F") & TX("p") & "\n");
  END Title;

PROCEDURE Format(source : TEXT;
                 mode : FM;
                 resp, ss : LONGREAL; 
                 df : INTEGER;
                 residual : LONGREAL;
                 residualDf : INTEGER) =

  VAR
    mss := ss/FLOAT(df,LONGREAL);
    residualMSS := residual/FLOAT(residualDf,LONGREAL);
    F := mss/residualMSS;
    p := 1.0d0-drdist.Fish(F,df,residualDf);
  BEGIN
    VAR
      respT : TEXT;
    BEGIN
      IF mode = FM.Full THEN
        respT := Num(resp) 
      ELSE
        respT := TX("")
      END;
      IO.Put(TX(" " & source, width := FirstWidth, align := Fmt.Align.Left) & respT & Num(ss) & 
                                           Int(df) & Num(mss));
    END;
    IF mode = FM.Full OR mode = FM.NoRes THEN
      IO.Put(Num(F) & Num(p,style := Style.Fix,hiPrec := TRUE));
      IF p < 0.01d0 THEN
        IO.Put("**")
      ELSIF p < 0.05d0 THEN
        IO.Put("*")
      END
    END;
    IO.Put("\n")
  END Format;

CONST TagColWidth = 100;

TYPE 
  MeanDumper = OBJECT
    stuff : LongrealPQ.T;
  METHODS
    init() : MeanDumper := MDI;
    formatMean(tag : TEXT; READONLY vals : ArrL; sortby : LONGREAL) := FMI;
    dump() := DDI;
  END;

  MDElt = LongrealPQ.Elt OBJECT
    t : TEXT
  END;

PROCEDURE MDI(md : MeanDumper) : MeanDumper =
  BEGIN 
    md.stuff := NEW(LongrealPQ.Default).init();
    RETURN md
  END MDI;

PROCEDURE DDI(md : MeanDumper) =
  <* FATAL LongrealPQ.Empty *>
  BEGIN
    WHILE md.stuff.size() > 0 DO
      IO.Put(NARROW(md.stuff.deleteMin(),MDElt).t)
    END
  END DDI;
  
PROCEDURE FMI(md : MeanDumper;
              tag : TEXT; READONLY values : ArrL; sortby : LONGREAL) =
  VAR
    tagWidth := TagColWidth - (NUMBER(values)-1)*PadWidth;
    t := "   ";
  BEGIN
     t:= t & (Fmt.Pad(tag, length := tagWidth-3, align := Fmt.Align.Left ));
    FOR i := FIRST(values) TO LAST(values) DO
      t := t & (Num(values[i]))
    END;
    t := t & ("\n");
    md.stuff.insert(NEW(MDElt, priority := sortby, t := t))
  END FMI;
  
PROCEDURE TitleAverages(READONLY titles : ArrT) =
  VAR
    tagWidth := TagColWidth - (NUMBER(titles)-1)*PadWidth;
  BEGIN
    IO.Put(Fmt.Pad("", length := tagWidth, align := Fmt.Align.Left ));
    FOR i := FIRST(titles) TO LAST(titles) DO
      IO.Put(TX(titles[i]))
    END;
    IO.Put("\n")
  END TitleAverages;

(***********************************************************************)

TYPE 
  MyFVI = ANOVA.FactorValueIterator OBJECT
    d : F;
    sortBy : Sortby;
    anova : ANOVA.T;
    mode : Mode;
    count := 0.0d0;
    dumper : MeanDumper;
  OVERRIDES
    callback := MFVICallback
  END;

PROCEDURE MFVICallback(fvi : MyFVI; 
                       READONLY factors, vals : ARRAY OF CARDINAL) =

  PROCEDURE FmtVals() : TEXT =
    VAR
      res := "";
    BEGIN
      FOR i := FIRST(factors) TO LAST(factors) DO
        res := res & RemQuotes(fvi.d.values[factors[i]].formatV(vals[i]));
        IF i # LAST(factors) THEN res := res & " . " END
      END;
      RETURN res
    END FmtVals;

  VAR
    df : CARDINAL;
    sortby : LONGREAL;
  BEGIN
      CASE fvi.mode OF 
        Mode.AllValues =>
        VAR seq := fvi.anova.allValues(factors,vals);
        BEGIN
          FOR i := 0 TO seq.size()-1 DO
            WITH val = seq.get(i) DO
              fvi.dumper.formatMean(FmtVals(), ArrL { val }, val)
            END
          END
        END
      |
        Mode.AllAverages =>
        VAR
          mean := fvi.anova.mean(factors,vals);
        BEGIN
          CASE fvi.sortBy OF
            Sortby.Mean => sortby := mean
          |
            Sortby.Min, Sortby.Max =>
            Debug.Error("Can't sort by min or max")
          |
            Sortby.None =>
            sortby := fvi.count
          END;
          fvi.dumper.formatMean(FmtVals(), ArrL { mean }, sortby)
        END
      |
        Mode.AllDevs =>
        VAR
          mean := fvi.anova.mean(factors,vals);
          sdev := fvi.anova.sdev(factors,vals,df);
        BEGIN
          CASE fvi.sortBy OF
            Sortby.Mean => sortby := mean
          |
            Sortby.Min => sortby := mean-2.0d0*sdev
          |
            Sortby.Max => sortby := mean+ 2.0d0*sdev
            
          |
            Sortby.None => sortby := fvi.count
          END;
          
          fvi.dumper.formatMean(FmtVals(), ArrL { mean, sdev } , sortby)
        END
      |
        Mode.MinMax =>
        VAR
          min := fvi.anova.min(factors,vals);
          median := fvi.anova.median(factors,vals);
          max := fvi.anova.max(factors,vals);
        BEGIN
          CASE fvi.sortBy OF
            Sortby.Mean => sortby := median
          |
            Sortby.Min => sortby := min
          |
            Sortby.Max => sortby := max
            
          |
            Sortby.None => sortby := fvi.count
          END;
          fvi.dumper.formatMean(FmtVals(), ArrL {min, median, max }, sortby)
        END
      |
        Mode.Confidence =>
        VAR 
          conf := fvi.anova.confidence(factors,vals,0.05d0);
          lsc := conf.lower;
          lpc := conf.lowerPooled;
          upc := conf.upperPooled;
          usc := conf.upper;
          mean := conf.mean;
        BEGIN
          CASE fvi.sortBy OF
            Sortby.Mean => sortby := mean
          |
            Sortby.Min => sortby := lsc
          |
            Sortby.Max => sortby := usc
            
          |
            Sortby.None => sortby := fvi.count
          END;
          fvi.dumper.formatMean(FmtVals(), ArrL { lsc, lpc, mean, upc, usc },
                            sortby)
        END
      END;
      fvi.count := fvi.count + 1.0d0
  END MFVICallback;

PROCEDURE PrintAverages2(d : F;
                         sortBy : Sortby;
                         mode : Mode;
                         anova : ANOVA.T;
                         READONLY factors : ARRAY OF CARDINAL) =
  VAR
    fmt := FormatFactors(d, factors);
    fvi := NEW(MyFVI,
               sortBy := sortBy,
               anova := anova,
               d := d,
               dumper := NEW(MeanDumper).init(),
               mode := mode);
  BEGIN
    IO.Put(" ");
    IO.Put(fmt);
    IO.Put("\n");

    anova.iterateAllFactorValues(factors, fvi);
    
    fvi.dumper.dump()

  END PrintAverages2;

PROCEDURE AnalyzeInteraction(anova : ANOVA.T;
                             READONLY factors : ARRAY OF CARDINAL;
                             residual : LONGREAL;
                             residualDf : INTEGER;
                             anaHighestAve : BOOLEAN;
                             d : F) =
  VAR
    o := NEW(REF ARRAY OF ANOVA.Order, anova.numVars());
    complex := FALSE;
  BEGIN
    FOR i := FIRST(o^) TO LAST(o^) DO
      o[i] := 0
    END;

    FOR i := FIRST(factors) TO LAST(factors) DO
      IF anova.levels(factors[i]) > 2 THEN
        o[factors[i]] := -1;
        complex := TRUE
      ELSE
        o[factors[i]] := 1
      END
    END;

    VAR
      fmt := FormatFactors(d, factors);
      totSS := anova.interactionSS(o^);
      df : CARDINAL := 1;
      n := FLOAT(anova.size(),LONGREAL);
      corr := 2.0d0 / Math.sqrt(n);
    BEGIN
      FOR i := FIRST(factors) TO LAST(factors) DO
        df := df * (anova.levels(factors[i])-1)
      END;
      IF complex THEN
        Format(fmt, FM.NoRes, 0.0d0,
               totSS, df, residual, residualDf)
      ELSE
        Format(fmt, FM.Full, 
               anova.interactionEffect(o^)*corr,
               totSS, df, residual, residualDf)

      END;
      IF NUMBER(factors) = 1 AND
         anova.levels(factors[0]) > 2 AND
         anova.isQuantitative(factors[0]) THEN

        Line('.');
        FOR p := 1 TO df DO
          o[factors[0]] := p;
          VAR
            ssp := anova.interactionSS(o^);
          BEGIN
            Format("Reduction to " & Power(p), FM.Full,
                   anova.interactionEffect(o^)*corr,
                   ssp, 1, residual, residualDf)
          END
        END
      END;

      IF NUMBER(factors) = 1 AND
        anova.levels(factors[0]) > 2 THEN
        Line('.');
        VAR
          lev := anova.levels(factors[0]);
          aves := NEW(RarrL, lev);
          sp : LONGREAL;
          c : CARDINAL;
        BEGIN
          FOR i := 0 TO lev-1 DO
            aves[i] := anova.mean(factors,ArrC {i})
          END;

          IF anaHighestAve THEN
            c := IndexOfHighest(aves^)
          ELSE
            c := IndexOfLowest(aves^)
          END;

          sp := anova.valueVersusAveEffect(factors[0],c);

          Format("\"" & RemQuotes(d.values[factors[0]].formatV(c)) & "\"",
                 FM.Full, sp * corr, sp*sp, 1, residual, residualDf);

          WITH values = d.values[factors[0]] DO
            VAR
              txt : TEXT := NIL;
            BEGIN
              FOR i := 0 TO values.n-1 DO
                IF i # c THEN
                  IF txt = NIL THEN
                    txt := ""
                  ELSE
                    txt := txt & ","
                  END;
                  txt := txt & "\"" & RemQuotes(values.formatV(i)) & "\""
                END
              END;
              IF Text.Length(txt) > FirstWidth - 3 THEN
                txt := Text.Sub(txt, 0, FirstWidth - 6) & " ..."
              END;
              Format(txt, FM.Error, 0.0d0, totSS - sp*sp, df-1, 0.0d0,0)
            END
          END
        END
      END
    END
  END AnalyzeInteraction;

PROCEDURE FormatFactors(d : F;
                        READONLY a : ARRAY OF CARDINAL) : TEXT =
  VAR
    fmt : TEXT;
  BEGIN
    IF NUMBER(a) = 1 THEN
      fmt := Fmt.Int(a[0])&"."&RemQuotes(d.values[a[0]].named)
    ELSE
      fmt := "";
      FOR i := FIRST(a) TO LAST(a) DO
        fmt := fmt & Fmt.Int(a[i]);
        IF i # LAST(a) THEN fmt := fmt & "." END
      END
    END;
    RETURN fmt
  END FormatFactors;

PROCEDURE IndexOfHighest(READONLY s : ARRAY OF LONGREAL) : CARDINAL =
  VAR
    res := -1;
    val := FIRST(LONGREAL);
  BEGIN
    FOR i := 0 TO LAST(s) DO
      IF s[i] > val THEN
        val := s[i];
        res := i
      END
    END;

    RETURN res
  END IndexOfHighest;

PROCEDURE IndexOfLowest(READONLY s : ARRAY OF LONGREAL) : CARDINAL =
  VAR
    res := -1;
    val := LAST(LONGREAL);
  BEGIN
    FOR i := 0 TO LAST(s) DO
      IF s[i] < val THEN
        val := s[i];
        res := i
      END
    END;

    RETURN res
  END IndexOfLowest;
  
PROCEDURE DoFactorialANOVAMode(anova : ANOVA.T;
                               rx : TEXT;
                               anaHighestAve : BOOLEAN;
                               d : F) =
  VAR
    mean, ss, residual, totalSS : LONGREAL;
    resDf : CARDINAL;
  BEGIN
    resDf := anova.size() - anova.slots();

    residual := anova.errorSS();
    totalSS := anova.totalSS();

    (* mean response *)
    mean := anova.mean();
    ss := mean*mean*FLOAT(anova.size(),LONGREAL);

    IO.Put(" Analysis of Variance \n");
    IO.Put(" Response expression: /" & rx & "/\n");
    IO.Put("\n");

    Title();
    Line('=');
    Format("mean", FM.Full,
           mean,ss,1,residual,resDf);

    VAR
      ss : LONGREAL;
      o := NEW(REF ARRAY OF ANOVA.Order, anova.numVars());
    BEGIN
      FOR i := FIRST(o^) TO LAST(o^) DO o[i] := 0 END;
      ss := anova.interactionSS(o^);
      
      (* singles *)
      FOR i := FIRST(o^) TO LAST(o^) DO
        o[i] := -1;
        ss := anova.interactionSS(o^);
        o[i] := 0
      END
    END;

    Line('-');

    <* FATAL IntPQ.Empty *>
    VAR
      idxs := anova.varyingIndices();
      iter := idxs.iterate();
      idx : INTEGER;
      pq := NEW(IntPQ.Default).init();
      tarr : RarrC;
    BEGIN
      WHILE iter.next(idx) DO
        pq.insert(NEW(IntPQ.Elt, priority := idx))
      END;
      
      VAR i := 0; 
      BEGIN
        tarr := NEW(RarrC, pq.size());

        WHILE pq.size() > 0 DO
          WITH pri = pq.deleteMin() DO
            tarr[i] := pri.priority; INC(i);

            AnalyzeInteraction(anova, ARRAY OF CARDINAL { pri.priority },
                               residual, resDf,
                               anaHighestAve,
                               d)
          END;
          Line('-')
        END
      END;

      (* compute power set! *)
      FOR sz := 2 TO NUMBER(tarr^) DO
        VAR
          iter := CardSubarrays.IterateOfSize(tarr,sz);
          arr := NEW(RarrC, sz);
        BEGIN
          WHILE iter.next(arr^) DO
            AnalyzeInteraction(anova,arr^,residual,resDf,
                               anaHighestAve,
                               d);
            Line('-')
          END
        END
      END
    END;

    Format("error", FM.Error,
           0.0d0, residual, resDf, residual, resDf);

    Line('=');

    Format("total", FM.Total,
           0.0d0, totalSS, anova.size(), residual, resDf);
  END DoFactorialANOVAMode;

PROCEDURE DoOtherMode(mode : Mode; 
                      anova : ANOVA.T; 
                      rx : TEXT; 
                      d : F; 
                      sortBy : Sortby) =
  BEGIN
    IO.Put(" All Responses \n");
    IO.Put(" Response expression: /" & rx & 
      "/\n");
    IO.Put("\n");

    CASE mode OF 
      Mode.AllValues =>
        TitleAverages(ArrT { "value" })
    |
      Mode.AllAverages =>
        TitleAverages(ArrT { "mean" })
    |
      Mode.AllDevs =>
        TitleAverages(ArrT { "mean", "sdev" })
    |
      Mode.MinMax =>
        TitleAverages(ArrT { "min", "median", "max" } )
    |
      Mode.Confidence =>
        TitleAverages(ArrT { "5%", "5% pooled", "mean", "95% pooled", "95%" })
    END;

    Line('=');
    <* FATAL IntPQ.Empty *>
    VAR
      idxs := anova.varyingIndices();
      iter := idxs.iterate();
      idx : INTEGER;
      pq := NEW(IntPQ.Default).init();
      tarr : RarrC;
      first := TRUE;
    BEGIN
      WHILE iter.next(idx) DO
        pq.insert(NEW(IntPQ.Elt, priority := idx))
      END;
     
      VAR i := 0; 
      BEGIN
        tarr := NEW(RarrC, pq.size());
        
        WHILE pq.size() > 0 DO
          WITH pri = pq.deleteMin() DO
            tarr[i] := pri.priority; INC(i)
          END
        END
      END;

      (* compute power set! *)
      FOR sz := 0 TO NUMBER(tarr^) DO
        VAR
          iter := CardSubarrays.IterateOfSize(tarr,sz);
          arr := NEW(RarrC, sz);
        BEGIN
          WHILE iter.next(arr^) DO
            IF first THEN first := FALSE ELSE Line('-') END;
            PrintAverages2(d, sortBy, mode, anova, arr^)
          END
        END
      END
    END;
    Line('=')
  END DoOtherMode;

BEGIN END ANOVAProgramBits.
