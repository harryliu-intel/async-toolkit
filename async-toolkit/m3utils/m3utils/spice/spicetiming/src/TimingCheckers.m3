MODULE TimingCheckers;
IMPORT Transition;
IMPORT TransitionSeq;
IMPORT CheckDir;
IMPORT TransitionFinder; FROM TransitionFinder IMPORT Index;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal;

CONST
  LR = LongReal;

VAR Verbose := Debug.DebugThis("TimingCheckers");

PROCEDURE MeasureSetup(clkIdx   : CARDINAL;
                       data     : TransitionSeq.T;
                       clk      : TransitionSeq.T;
                       dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the pulse timing setup for a signal relative to
     a leading (enabling) clock edge *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
    dataIdx : Index;
    dataTrans : Transition.T;
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    (* distrans is what defines the END OF THE CURRENT CYCLE *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);
    
    (* find previous transition of data *)
    dataIdx := TransitionFinder.FindFloorIdx(data, disTrans.at);

    (* now look for the direction of data transition we are sensitive to *)
    WHILE dataIdx # -1 DO
      dataTrans := data.get(dataIdx);
      IF dataTrans.dir IN dataDir THEN EXIT END;
      DEC(dataIdx)
    END;

    IF dataIdx = -1 THEN
      RETURN Fail
    END;

    (* find previous transition of clock and check whether data transition
       was after *)
    
    IF clkIdx # 0 THEN
      WITH prevDis = clk.get(clkIdx - 1) DO
        IF dataTrans.at <= prevDis.at THEN
          (* there were no transitions on this cycle *)
          RETURN Fail
        END
      END
    END;

    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH setupTime = clkTrans.at - dataTrans.at DO
      IF Verbose THEN
        Debug.Out(F("Measured setup from edge at %s, setup %s",
                    LR(clkTrans.at), LR(setupTime)))
      END;
      RETURN setupTime
    END
  END MeasureSetup;

PROCEDURE MeasurePulsemargin(clkIdx   : CARDINAL;
                             data     : TransitionSeq.T;
                             clk      : TransitionSeq.T;
                             dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the pulse margin from a flipping internal node
     to a flipping clock *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
    dataIdx : Index;
    dataTrans : Transition.T;
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    (* distrans is what defines the END OF THE CURRENT CYCLE *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);
    
    (* find previous transition of data -- this is the interesting transition of
       the aggressor in the case of the pulse margin *)
    dataIdx := TransitionFinder.FindFloorIdx(data, disTrans.at);

    (* now look for the direction of data transition we are sensitive to *)
    WHILE dataIdx # -1 DO
      dataTrans := data.get(dataIdx);
      IF dataTrans.dir IN dataDir THEN EXIT END;
      DEC(dataIdx)
    END;

    IF dataIdx = -1 THEN
      RETURN Fail
    END;

    (* check whether data transition
       was after RISING edge of clock (same cycle) *)
    
    IF dataTrans.at <= clkTrans.at THEN
      (* there were no interesting transitions on this cycle *)
      RETURN Fail
    END;

    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH pulseMargin = disTrans.at - dataTrans.at DO
      IF Verbose THEN
        Debug.Out(F("Measured pulsemargin from edge at %s, setup %s",
                    LR(clkTrans.at), LR(pulseMargin)))
      END;
      RETURN pulseMargin
    END
  END MeasurePulsemargin;

PROCEDURE MeasureHold(clkIdx   : CARDINAL;
                      data     : TransitionSeq.T;
                      clk      : TransitionSeq.T;
                      dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the pulse timing setup for a signal relative to
     a leading (enabling) clock edge *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
    dataIdx : Index;
    dataTrans : Transition.T;
    nTrans := data.size();
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);

    (* find next transition after enabling edge *)
    (* first find previous transition of data before enabling edge *)
    dataIdx := TransitionFinder.FindFloorIdx(data, clkTrans.at);

    INC(dataIdx);

    (* now we point to the next transition *)

    
    (* now look for the direction of data transition we are sensitive to *)
    WHILE dataIdx # nTrans DO
      dataTrans := data.get(dataIdx);
      IF dataTrans.dir IN dataDir THEN EXIT END;
      INC(dataIdx)
    END;

    IF dataIdx = nTrans THEN
      RETURN Fail
    END;

    (* find next transition of clock and check whether data transition
       was after *)
    
    IF disIdx # clk.size() - 1 THEN
      WITH nextEna = clk.get(disIdx + 1) DO
        IF dataTrans.at > nextEna.at THEN
          (* there were no transitions on this cycle *)
          RETURN Fail
        END
      END
    END;

    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH holdTime = dataTrans.at - disTrans.at DO
      IF Verbose THEN
        Debug.Out(F("Measured hold from edge at %s, hold %s",
                    LR(clkTrans.at), LR(holdTime)))
      END;
      RETURN holdTime
    END
  END MeasureHold;

PROCEDURE MeasurePulsewidth(clkIdx   : CARDINAL;
                            <*UNUSED*>data     : TransitionSeq.T;
                            clk      : TransitionSeq.T;
                            <*UNUSED*>dataDir  : CheckDir.T) : LONGREAL =

  (* measure pulse width of clock pulse *)
  VAR
    clkTrans := clk.get(clkIdx);
    disIdx   : CARDINAL;
    disTrans : Transition.T;
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);


    (* we had a transition that we are sensitive to DURING the cycle *)
    WITH pulseTime = disTrans.at - clkTrans.at DO
      IF Verbose THEN
        Debug.Out(F("Measured pulse from edge at %s, pulse %s",
                    LR(clkTrans.at), LR(pulseTime)))
      END;
      RETURN pulseTime
    END
  END MeasurePulsewidth;

PROCEDURE MeasureGlitchwidth(clkIdx   : CARDINAL;
                             data     : TransitionSeq.T;
                             clk      : TransitionSeq.T;
                             <*UNUSED*>dataDir  : CheckDir.T) : LONGREAL =

  (* here we are measuring the 1/length of the glitchy period before an active edge of
the clock *)

  VAR
    clkTrans := clk.get(clkIdx);
    disIdx       : CARDINAL;
    disTrans     : Transition.T;

    prvIdx       : Index;
    prvTrans     : Transition.T;
    
    data0Idx     : Index;
    data0Trans   : Transition.T;

    data1Idx     : Index;
    data1Trans   : Transition.T;

    nData := data.size();
    
  BEGIN
    (* find disabling edge of clock, if none, we can't measure *)
    (* distrans is what defines the END OF THE CURRENT CYCLE *)
    disIdx := clkIdx + 1;
    IF disIdx = clk.size() THEN RETURN Fail END;

    (* find disabling transition *)
    disTrans := clk.get(disIdx);

    (* find previous clock edge *)
    prvIdx := clkIdx - 1;
    IF prvIdx = -1 THEN RETURN Fail END;

    prvTrans := clk.get(prvIdx);
    
    (* find previous transition of data -- this is the LAST transition we care about *)
    data1Idx := TransitionFinder.FindFloorIdx(data, disTrans.at);

    data0Idx := TransitionFinder.FindFloorIdx(data, prvTrans.at) + 1;

    IF data1Idx = -1 OR data0Idx = 0 OR data0Idx = nData THEN
      RETURN Fail
    END;

    data1Trans := data.get(data1Idx);
    data0Trans := data.get(data0Idx);

    IF Verbose THEN
      Debug.Out(F("disTrans.at=%s prvTrans.at=%s data0Trans.at=%s data1Trans.at=%s",
                  LR(disTrans.at), LR(prvTrans.at),
                  LR(data0Trans.at), LR(data1Trans.at)))
    END;

    IF data0Trans.at < prvTrans.at OR data1Trans.at < prvTrans.at THEN
      RETURN Fail
    END;
    
    VAR
      glitchWidth := data1Trans.at - data0Trans.at;
      ret : LONGREAL;
    BEGIN
      IF glitchWidth = 0.0d0 THEN
        ret := LAST(LONGREAL)
      ELSE
        ret := 1.0d0/glitchWidth
      END;

      IF Verbose THEN
        Debug.Out(F("Measured glitchwidth from edge at %s, glitchwidth %s ret %s",
                    LR(clkTrans.at), LR(glitchWidth), LR(ret)))
      END;
      RETURN ret
    END
  END MeasureGlitchwidth;

BEGIN END TimingCheckers.
