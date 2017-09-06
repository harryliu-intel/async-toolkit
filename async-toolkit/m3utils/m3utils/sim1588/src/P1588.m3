
MODULE P1588 EXPORTS Main;

(* 
   1588 Vernier Timestamp Unit simulation
   
   Author: Mika Nystrom <mika.nystroem@intel.com>
   
   May, 2015 
*)

IMPORT LongrealPQ;
IMPORT Clock, ClockSpec;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Debug;
IMPORT LongRealSeq;
IMPORT Fmt;
IMPORT Tyme;
IMPORT Scan, Params;
IMPORT FloatMode, Lex;
IMPORT Euclid;
IMPORT Wr, FileWr;
IMPORT Math;
IMPORT Thread;

<*FATAL Thread.Alerted, Wr.Failure*>
<*FATAL FloatMode.Trap, Lex.Error*>

CONST
  DoDebug = FALSE;
  (* clocks *)
  (* F[0] is the 1588 clock; the others are link clocks *)

TYPE 
  CType = { Ref, Eth };

VAR
  CS    : ARRAY CType OF ClockSpec.T;

TYPE
  Event = LongrealPQ.Elt OBJECT
    clock : MyClock;
  END;

  P1588Callback = Clock.Callback OBJECT
    state : ARRAY [VAL(1,CType)..LAST(CType)] OF ClockState;
  METHODS
    init(minPhaseSpan : CARDINAL) : P1588Callback := PCInit;
  OVERRIDES
    do := P1588Do;
  END;

PROCEDURE PCInit(cb : P1588Callback; minPhaseSpan : CARDINAL) : P1588Callback =
  BEGIN
    FOR i := FIRST(cb.state) TO LAST(cb.state) DO
      InitState(cb.state[i]);
      cb.state[i].minPhaseSpan := minPhaseSpan
    END;
    RETURN cb
  END PCInit;
    
PROCEDURE P1588Do(c : P1588Callback; tm : Tyme.T) =
  BEGIN 
    Clock.Callback.do(c, tm);

    FOR i := VAL(1,CType) TO LAST(CType) DO
      WITH err = ProcessEth(c, tm, i) DO
        AccumRes(clocks[FIRST(clocks)].seq.size()-1,Math.sqrt(err*err))
      END
    END
  END P1588Do;

TYPE
  ClockState = RECORD (* hardware state*)
    lastCount   : INTEGER;
    lastTime    : Tyme.T;

    sumNum      : CARDINAL;
    sumDenom    : CARDINAL;

    minPhase    : INTEGER;
    minPhaseAge : CARDINAL;

    minPhase2   : INTEGER;
    minPhase2Age: CARDINAL;

    minPhaseSpan: CARDINAL;
  END;

PROCEDURE InitState(VAR st : ClockState) =
  (* initialize hardware state *)
  BEGIN
    st.lastCount   := 0;
    st.lastTime    := FIRST(Tyme.T);

    st.sumNum      := 0;
    st.sumDenom    := 0;

    st.minPhase    := LAST(INTEGER);
    st.minPhaseAge := 0;

    st.minPhase2   := LAST(INTEGER);
    st.minPhase2Age:= 0;
  END InitState;

PROCEDURE ProcessEth(c : P1588Callback; tm : Tyme.T; whch : CType) : LONGREAL =
  (* model of the hardware *)
  VAR
    deltaT : Tyme.T;
  BEGIN
    WITH s        = c.state[whch],
         cs       = CS[whch],
         newCount = clocks[whch].seq.size(),
         incr     = newCount - s.lastCount,
         incrN    =        cs.n,
         incrD    = incr * cs.d DO

      IF newCount # 0 THEN
        deltaT := tm - clocks[whch].seq.gethi()
      ELSE
        deltaT := 0.0d0 (* not quite *)
      END;

      INC(s.sumNum,   incrN);
      INC(s.sumDenom, incrD);

      WITH phase = s.sumNum - s.sumDenom DO

        IF s.minPhaseAge = s.minPhaseSpan THEN
          (* start collecting backup minPhase *)
          s.minPhase2    := phase;
          s.minPhase2Age := 0
        ELSIF phase < s.minPhase2 THEN
          (* update backup minPhase *)
          s.minPhase2    := phase;
          s.minPhase2Age := 0
        ELSE
          INC(s.minPhase2Age)
        END;

        IF s.minPhaseAge = 2*s.minPhaseSpan THEN
          (* weve hit end of life for minPhase, replace it with newer vers. *)
          <*ASSERT s.minPhase2Age < s.minPhaseAge*>
          s.minPhase     := s.minPhase2;
          s.minPhaseAge  := s.minPhase2Age;
          s.minPhase2    := phase;
          s.minPhase2Age := 0
        END;

        IF phase < s.minPhase THEN
          s.minPhase    := phase; 
          s.minPhaseAge := 0
        ELSE
          INC(s.minPhaseAge)
        END;

        WITH offPhase = phase - s.minPhase,
             corrDT   = FLOAT(offPhase,LONGREAL) / 
                          FLOAT(cs.n,LONGREAL) / 
                            CS[FIRST(CType)].f,
             errDT    = corrDT - deltaT
          
         DO
                
          IF DoDebug THEN
            Debug.Out(F(" %s incrN %s incrD %s sumN %s sumD %s",
                        Int(ORD(whch)), 
                      Int(incrN), Int(incrD), Int(s.sumNum), Int(s.sumDenom)) &
                      F(" %s deltaT %s phase %s minPhase %s", 
                        LongReal(tm), LongReal(deltaT),
                        Int(phase), Int(s.minPhase)) &
                        F(" minPhaseAge %s minPhase2Age %s offPhase %s corrDT %s errDT %s", 
                          Int(s.minPhaseAge), Int(s.minPhase2Age),
                          Int(offPhase), LongReal(corrDT), LongReal(errDT)))
          END;
          s.lastCount := newCount;
          RETURN errDT
        END
      END;

    END
  END ProcessEth;

TYPE
  MyClock = Clock.T OBJECT
    exact : LongRealSeq.T;
  END;

VAR 
  eventQ       : LongrealPQ.T;
  clocks       : ARRAY [FIRST(CS)..LAST(CS)] OF MyClock;
  time                    := 0.0d0;
  baseF        : Tyme.T   := Scan.LongReal(Params.Get( 1));
  eNum         : CARDINAL := Scan.Int     (Params.Get( 2));
  eDen         : CARDINAL := Scan.Int     (Params.Get( 3));
  rNum         : CARDINAL := Scan.Int     (Params.Get( 4));
  rDen         : CARDINAL := Scan.Int     (Params.Get( 5));
  err          : Tyme.T   := Scan.LongReal(Params.Get( 6));
  minPhaseSpan : CARDINAL := Scan.Int     (Params.Get( 7));
  jitter       : Tyme.T   := Scan.LongReal(Params.Get( 8));
  Samples      : CARDINAL := Scan.Int     (Params.Get( 9));
  Rcycles      : CARDINAL := Scan.Int     (Params.Get(10));

PROCEDURE Freq(n, d : CARDINAL; b : Tyme.T) : Tyme.T =
  BEGIN
    RETURN FLOAT(n,Tyme.T) / FLOAT(d, Tyme.T) * b
  END Freq;

PROCEDURE RunOneSim() =
  BEGIN
    eventQ := NEW(LongrealPQ.Default).init();
    FOR i := FIRST(CS) TO LAST(CS) DO
      IF DoDebug THEN
        Debug.Out(F("clk[%s] %s MHz +/- %s p.p.m.",
                    Int(ORD(i)), 
                    LongReal(CS[i].f/1.0d6), 
                    LongReal(CS[i].e * 1.0d6)))
      END;
      clocks[i] := NEW(MyClock).init(CS[i], 
                                     nm     := F("clk[%s]",Int(ORD(i))),
                                     jitter := jitter);
      IF i = FIRST(CS) THEN
        clocks[i].m := NEW(P1588Callback).init(minPhaseSpan)
      ELSE
        clocks[i].m := NEW(Clock.Callback)
      END;
      clocks[i].m.clock := clocks[i];
      clocks[i].exact   := NEW(LongRealSeq.T).init()
    END;

    (* invariant: each clock appears exactly once in event queue *)
    (* initialize event queue *)
    FOR i := FIRST(clocks) TO LAST(clocks) DO
      WITH tm = clocks[i].nextEdge(),
           elt = NEW(Event, priority := tm, clock := clocks[i]) DO
        eventQ.insert(elt)
      END
    END;

    WHILE clocks[FIRST(clocks)].seq.size() < Rcycles DO
      WITH min = NARROW(eventQ.deleteMin(),Event) DO
        (* set time *)
        time := min.priority;

        (* calculate exact(??) P1588 time *)
        WITH exact = CS[FIRST(CS)].f * time DO
          IF DoDebug THEN
            Debug.Out(F("%14s ns : %7s %7s P1588exact %13s", 
                        LongReal(time/1.0d-9, prec := 6, style := Fmt.Style.Fix), 
                        min.clock.getNm(),
                        Int(min.clock.seq.size()),
                        LongReal(exact,prec := 6, style := Fmt.Style.Fix)))
          END;
          min.clock.exact.addhi(exact);
        END;

        min.clock.m.do(time);
        
        (* generate next event *)
        WITH nextTm = min.clock.nextEdge(),
             elt    = NEW(Event, priority := nextTm, clock := min.clock) DO
          eventQ.insert(elt)
        END

        (* process event *)
      END
    END
  END RunOneSim;

VAR
  stats : REF ARRAY OF Stats;

TYPE
  Stats = RECORD
    min           := LAST (LONGREAL);
    max           := FIRST(LONGREAL);
    n, sum, sumSq := 0.0d0;
  END;

PROCEDURE AccumRes(idx : INTEGER; val : LONGREAL) =
  BEGIN
    WITH s = stats[idx] DO
      s.min   := MIN(s.min, val);
      s.max   := MAX(s.max, val);
      s.n     := s.n + 1.0d0;
      s.sum   := s.sum + val;
      s.sumSq := s.sumSq + val*val
    END
  END AccumRes;

PROCEDURE DumpOut() =
  VAR
    wr : Wr.T;
    ofn := "out";
  BEGIN
    FOR i := 1 TO 10 DO
      ofn := ofn & "_" & Params.Get(i)
    END;
    ofn := ofn & ".dat";
    
    wr := FileWr.Open(ofn);

    FOR i := FIRST(stats^) TO LAST(stats^) DO
      WITH s = stats[i] DO
        WITH mean = s.sum / s.n,
             sdev = Math.sqrt(s.sumSq / s.n - mean*mean) DO
          Wr.PutText(wr, F("%s %s %s %s %s",
                           Int(i),
                           LongReal(mean),
                           LongReal(sdev),
                           LongReal(s.min),
                           LongReal(s.max)));
          FOR i := 1 TO 10 DO
            Wr.PutChar(wr, ' ');
            Wr.PutText(wr, Params.Get(i))
          END;
          Wr.PutChar(wr, '\n')
        END
      END
    END;
    
    Wr.Close(wr)
  END DumpOut;

BEGIN
  stats := NEW(REF ARRAY OF Stats, Rcycles);

  VAR n,d : INTEGER; BEGIN  
    Euclid.Lowest(eNum,eDen,n,d); eNum := n; eDen := d 
  END;

  VAR n,d : INTEGER; BEGIN  
    Euclid.Lowest(rNum,rDen,n,d); rNum := n; rDen := d 
  END;

  CS[CType.Ref] := ClockSpec.T { Freq(rNum, rDen, baseF), err, 1, 1 };

  VAR
    n, d : INTEGER;
  BEGIN
    Euclid.Lowest(eNum*rDen, rNum*eDen, n, d);

    CS[CType.Eth] := ClockSpec.T { Freq(eNum, eDen, baseF), err, d, n };
  END;

  FOR i := 0 TO Samples-1 DO
    RunOneSim()
  END;

  DumpOut()
END P1588.
