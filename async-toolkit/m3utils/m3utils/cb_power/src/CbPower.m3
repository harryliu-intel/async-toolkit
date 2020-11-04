MODULE CbPower EXPORTS Main;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F, Int, FN;
IMPORT Math;
IMPORT Text;
IMPORT Wr, FileWr;

CONST LR  = Fmt.LongReal;
      TE  = Text.Equal;
      
TYPE
  Clock = {
  Bps,
  TmBps,
  Pps,
  Mclk,
  SerDesClk
  };
  
CONST
  ClockNames = ARRAY Clock OF TEXT {
  "Bps",
  "TmBps",
  "Pps",
  "Mclk",
  "SerDesClk"
  };

TYPE
  CA = ARRAY Clock OF LONGREAL;

  BlockInfo = RECORD
    name              : TEXT;
    clocks            : CA;
    area              : REAL;
    weight            : CARDINAL;
    hasCustomP := FALSE;
    customP    := 0.0d0;
    size           := 1.0;
    weightedArea   := 0.0d0;
  END;
  
CONST
  Data = ARRAY OF BlockInfo {
  BlockInfo { name   := "Parser",
              clocks := CA { 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 13.59,
              weight := 4
  },
  BlockInfo { name   := "Deparser",
              clocks := CA { 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 12.98,
              weight := 4
  },
  BlockInfo { name   := "LearnEtc",
              clocks := CA { 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 7.99,
              weight := 4
  },
  BlockInfo { name   := "MauPipes",
              clocks := CA { 0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0 },
              area   := 48.3,
              weight := 4,
              size   := 20.0
  },
  BlockInfo { name   := "PipeTmIntf",
              clocks := CA { 0.5d0, 0.5d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 11.23,
              weight := 1
  },
  BlockInfo { name   := "PacketStorage",
              clocks := CA { 0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 62.8,
              weight := 1,
              size   := 96.0
  },
  BlockInfo { name   := "QueueingLogic",
              clocks := CA { 0.0d0, 0.4d0, 0.6d0, 0.0d0, 0.0d0 },
              area   := 95.01,
              weight := 1
  },
  BlockInfo { name   := "MacPcs",
              clocks := CA { 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0 },
              area   := 22.88,
              weight := 1
  },
  BlockInfo { name   := "Misc",
              clocks :=   CA { 0.2d0, 0.2d0, 0.2d0, 0.2d0, 0.2d0 },
              area   := 56.49,
              weight := 1
  },
  BlockInfo { name   := "Serdes",
              clocks :=    CA { 0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0 }, 
              area   := 235.0,
              weight := 1,
              
              hasCustomP := TRUE,
              customP    := BaseSerdesP
  }
  };

CONST
  (*
     The base power figures are a bit confusing here.

     They are not used to compute SERDES power.  

     They are just used to set up the base power for the two modes

     56G overall power is 503.0 -  6.4 = 496.6 W

     112G                 496.6 - 37.5 = 459.1 W

  *)
  BaseP56G       = 503.0d0 - 6.4d0;
  (* the 6.4W adjustment comes from Ram's update on the SERDES power
     10/19/20 *)
  
  BaseP112Gvs56G = 25.0d0 * 1.5d0;
  BaseP112G      = BaseP56G - BaseP112Gvs56G;

  (**********************************************************************)
  
  BaseLeakageP   = 30.0d0;
  BasePackBytes  = 247;
  BaseV          = 0.75d0;

  BaseSerdesEpb  = 7.0d-12;
  (* serdes power is 700 mW per lane per Ram 10/19/20
     down from 750 mW *)
  
  BaseSerdesbps  = 12.8d12;
  BaseSerdesP    = BaseSerdesEpb * BaseSerdesbps;

  BaseDynP       = BaseP112G - BaseLeakageP;

  BaseInterDieP  = 20.0d0;

  BaseClks       = ARRAY Clock OF LONGREAL {
  1.35d0,
  1.505d0,
  1.5d0,
  1.0d0,
  1.0d0
  };

  ArchSlowClks       = ARRAY Clock OF LONGREAL {
  1.3d0,
  1.3d0,
  1.25156d0,
  1.0d0,
  1.0d0
  };

  ArchSlowPps1Clks       = ARRAY Clock OF LONGREAL {
  1.3d0,
  1.3d0,
  1.0d0,
  1.0d0,
  1.0d0
  };

  BaseVs         = ARRAY Clock OF LONGREAL {
  BaseV, ..
  };

  Reduced20Vs        = ARRAY Clock OF LONGREAL {
  BaseV - 0.020d0, ..
  };
  
  Reduced30Vs        = ARRAY Clock OF LONGREAL {
  BaseV - 0.030d0, ..
  };

  Reduced50Vs        = ARRAY Clock OF LONGREAL {
  BaseV - 0.050d0, ..
  };

  PpsLowV            = ARRAY Clock OF LONGREAL {
  Reduced50Vs[Clock.Bps],
  Reduced50Vs[Clock.TmBps],
  Reduced50Vs[Clock.Pps] - 0.050d0,
  Reduced50Vs[Clock.Mclk],
  Reduced50Vs[Clock.SerDesClk]
  };
  
  TmPpsLowV            = ARRAY Clock OF LONGREAL {
  Reduced50Vs[Clock.Bps],
  Reduced50Vs[Clock.TmBps] - 0.050d0,
  Reduced50Vs[Clock.Pps] - 0.050d0,
  Reduced50Vs[Clock.Mclk],
  Reduced50Vs[Clock.SerDesClk]
  };
  
  NB = NUMBER(Data);

PROCEDURE LR2(x : LONGREAL) : TEXT =
  BEGIN
    RETURN LR(x, style := Fmt.Style.Fix, prec := 2);
  END LR2;
  
PROCEDURE LR4(x : LONGREAL) : TEXT =
  BEGIN
    RETURN LR(x, style := Fmt.Style.Fix, prec := 4);
  END LR4;

PROCEDURE FindBlock(nm : TEXT) : [0..NB-1] =
  BEGIN
    FOR i := FIRST(Data) TO LAST(Data) DO
      IF TE(Data[i].name, nm) THEN RETURN i END
    END;
    Debug.Error(F("No block named \"%s\"", nm));
    <*ASSERT FALSE*>
  END FindBlock;
  
VAR
  blocks : ARRAY [0..NB-1] OF BlockInfo := Data;
  totalUnweightedP, totalWeightedArea := 0.0d0;
  totalWeightedP, dynPPerUnitArea : LONGREAL;

  blockP : ARRAY [0..NB-1] OF LONGREAL;

PROCEDURE CalcDynP(READONLY clk : ARRAY Clock     OF LONGREAL;
                   READONLY v   : ARRAY Clock     OF LONGREAL;
                   READONLY sz  : ARRAY [0..NB-1] OF LONGREAL) : LONGREAL =
  VAR
    clkTot := ARRAY Clock OF LONGREAL { 0.0d0 , .. };
    tot, this : LONGREAL;
  BEGIN
    FOR i := FIRST(blocks) TO LAST(blocks) DO
      WITH b                     = blocks[i],
           szRatio               = sz[i] / FLOAT(b.size,LONGREAL) DO
        this := 0.0d0;
        IF b.hasCustomP THEN
          this := b.customP;
          FOR c := FIRST(b.clocks) TO LAST(b.clocks) DO
            clkTot[c] := clkTot[c] + b.clocks[c] * b.customP
          END
        ELSE
          VAR
            baselineBlockP := blockP[i];
          BEGIN
            FOR c := FIRST(clk) TO LAST(clk) DO
              WITH baseBlockClkP         = baselineBlockP * b.clocks[c],
                   clkRatio              = clk[c] / BaseClks[c],
                   speedBlockClkP        = baseBlockClkP  * clkRatio,
                   vddRatio              = v[c] / BaseV,
                   speedVoltageBlockClkP = speedBlockClkP * vddRatio * vddRatio,
                   incrP                 = speedVoltageBlockClkP * szRatio
               DO
                this := this + incrP;
                clkTot[c] := clkTot[c] + incrP
              END
            END
          END
        END(*FI*);
        Debug.Out(F("block %-15s P %10s", b.name, LR2(this)));
        tot := tot + this
      END
    END;
    Debug.Out("-------------------------------");
    FOR c := FIRST(Clock) TO LAST(Clock) DO
      Debug.Out(F("clock %-15s P %10s", ClockNames[c], LR2(clkTot[c])));
    END;
    RETURN tot
  END CalcDynP;

PROCEDURE LeakageRatio(v            : LONGREAL) : LONGREAL =
  (* assume quadratic for now *)
  BEGIN
    WITH vr = v / BaseV DO
      RETURN Math.pow(vr, 2.0d0)
    END
  END LeakageRatio;
      
PROCEDURE CalcLeakageP(READONLY v : ARRAY Clock OF LONGREAL;
                       READONLY sz  : ARRAY [0..NB-1] OF LONGREAL) : LONGREAL =
  VAR
    totLkgP := 0.0d0;
  BEGIN
    FOR i := FIRST(blocks) TO LAST(blocks) DO
      WITH b       = blocks[i],
           szRatio               = sz[i] / FLOAT(b.size,LONGREAL),
           pctA    = b.weightedArea / totalA, 
           baseLkP = pctA * BaseLeakageP DO
        FOR c := FIRST(v) TO LAST(v) DO
          WITH p           = blocks[i].clocks[c],
               baseClkLkgP = baseLkP * p,
               lkgP        = LeakageRatio(v[c]) * baseClkLkgP * szRatio DO
            totLkgP := totLkgP + lkgP 
          END
        END
      END
    END;
    RETURN totLkgP
  END CalcLeakageP;
  
PROCEDURE DoScenario(nm               : TEXT;
                     READONLY clk, v  : ARRAY Clock     OF LONGREAL;
                     READONLY sz      : ARRAY [0..NB-1] OF LONGREAL) =
  VAR
    totDynP : LONGREAL;
    fmt := F("%-15s %s %s", nm, "%-25s", "%12s");
  BEGIN
    Debug.Out(F("scenario %s >>>>>>>>>>>>>>>>>", nm));
    totDynP := CalcDynP(clk, v, sz);
    Debug.Out(F(fmt, "totDynP/die [112G]", LR2(totDynP)));
    WITH lkgP = CalcLeakageP(v, sz),
         totP = totDynP + lkgP,
         tot2P = 2.0d0 * totP + BaseInterDieP DO
      Debug.Out(F(fmt, "lkgP/die [112G]", LR2(lkgP)));
      Debug.Out(F(fmt, "totP/die [112G]", LR2(totP)));
      Debug.Out(F(fmt, "totP/2die [112G]", LR2(tot2P)));
    END;
    

    Debug.Out(F("scenario %s <<<<<<<<<<<<<<<<<", nm));
  END DoScenario;

PROCEDURE DoSimpleModel() =
  CONST
    Pppsmax       =  348.0d0;
    Pserdesconst  =  110.0d0;
    Prestmax      =  470.0d0;

    Vminsram      = 0.600d0;
    Vminrest      = 0.75d0;

    Fppsmax       = 1.50d0;
    Apsmin        = 247;
    Vmax          = 0.80d0;
    
  VAR
    Vf, Ppsclock : LONGREAL;
    wr := FileWr.Open("simple.dat");
    
  PROCEDURE V(f : LONGREAL) : LONGREAL =
    BEGIN
      (* linear, based on 0.80V @ 1.5, 0.70V @ 1.25 *)
      RETURN 0.55d0 / 1.5d0 * f + 0.25d0
    END V;
    
  PROCEDURE Ppps() : LONGREAL =
    BEGIN
      WITH vpps = MAX(Vminsram, Vf),
           vratio = vpps / Vmax,
           fratio = Ppsclock / Fppsmax,
           p      = Pppsmax * Multiplier(vratio,fratio) DO
        Wr.PutText(wr, F(",%s", LR(Ppsclock)));
        Wr.PutText(wr, F(",%s", LR(vpps)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END Ppps;

  PROCEDURE PppsSingle() : LONGREAL =
    BEGIN
      WITH vpps = MAX(Vminrest, MAX(Vminsram, Vf)),
           vratio = vpps / Vmax,
           fratio = Ppsclock / Fppsmax,
           p      = Pppsmax * Multiplier(vratio,fratio) DO
        Wr.PutText(wr, F(",%s", LR(Ppsclock)));
        Wr.PutText(wr, F(",%s", LR(vpps)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END PppsSingle;

  PROCEDURE PppsLow() : LONGREAL =
    BEGIN
      WITH vpps = Vf,
           vratio = vpps / Vmax,
           fratio = Ppsclock / Fppsmax,
           p      = Pppsmax * Multiplier(vratio,fratio) DO
        Wr.PutText(wr, F(",%s", LR(Ppsclock)));
        Wr.PutText(wr, F(",%s", LR(vpps)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END PppsLow;

  PROCEDURE Pserdes() : LONGREAL =
    BEGIN
      WITH p = Pserdesconst DO
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END Pserdes;

  PROCEDURE Prest() : LONGREAL =
    BEGIN
      WITH vrest = MAX(Vminrest, Vf),
           vratio = vrest / Vmax,
           p      = Prestmax * Multiplier(vratio, 1.0d0) DO
        Wr.PutText(wr, F(",%s", LR(vrest)));
        Wr.PutText(wr, F(",%s", LR(p)));
        RETURN p
      END
    END Prest;
    
  BEGIN
    Wr.PutText(wr, "aps, fpps, vpps, ppps, fppsSingle, vppsSingle, pppsSingle, fppsLow, vppsLow, pppsLow, pserdes, vrest, prest, ptot, ptotSingle, ptotLow\n");
    FOR aps := 247 TO 622 (* 622 -> 595 MHz *) DO
      Wr.PutText(wr, F("%s", Int(aps)));
      
      Ppsclock := Fppsmax * FLOAT(Apsmin,LONGREAL)/FLOAT(aps,LONGREAL);
      Vf := V(Ppsclock);
      WITH p1 = Ppps(),
           p1s= PppsSingle(),
           p1l= PppsLow(),
           p2 = Pserdes(),
           p3 = Prest(),
           
           sum       = p1 + p2 + p3,  (* baseline assumption, SRAM Vmin limit *)
           sumSingle = p1s + p2 + p3, (* single power domain calc *)
           sumLow    = p1l + p2 + p3  (* assume NOT SRAM limited *)
       DO 
        Debug.Out(FN("%s %s %s %s %s %s %s",
                     ARRAY OF TEXT {
                    LR(FLOAT(aps,LONGREAL)),
                    LR(Ppsclock),
                    LR(Vf),
                    LR(p1),
                    LR(p2),
                    LR(p3),
                    LR(sum)} ));
        Wr.PutText(wr, F(",%s", LR(sum)));
        Wr.PutText(wr, F(",%s", LR(sumSingle)));
        Wr.PutText(wr, F(",%s", LR(sumLow)));
        Wr.PutChar(wr, '\n')
      END
    END;
    Wr.Close(wr)
  END DoSimpleModel;

PROCEDURE Multiplier(vratio, fratio : LONGREAL) : LONGREAL =
  BEGIN
    RETURN vratio * vratio * fratio
  END Multiplier;
  
VAR
  totalA := 0.0d0; (* total area of all accounted-for blocks *)
  BaseSizes, RedSizes, RedMauSizes : ARRAY [0..NB-1] OF LONGREAL;
BEGIN

  Debug.Out("====================  BASELINE SETUP  ====================");
  FOR i := FIRST(blocks) TO LAST(blocks) DO
    WITH b = blocks[i] DO
      BaseSizes[i] := FLOAT(b.size,LONGREAL);
      b.weightedArea :=
          FLOAT(b.area,LONGREAL) * FLOAT(b.weight, LONGREAL);
      Debug.Out(F("block %-15s weightedArea %10s",
                  b.name,
                  LR2(b.weightedArea)));

      totalA := totalA + b.weightedArea;
      
      IF b.hasCustomP THEN
        totalUnweightedP := totalUnweightedP + b.customP
      ELSE
        totalWeightedArea := totalWeightedArea + b.weightedArea
      END;
    END
  END;

  totalWeightedP := BaseDynP - totalUnweightedP;
  dynPPerUnitArea   := totalWeightedP / totalWeightedArea;

  Debug.Out(F("totalWeightedArea    %10s", LR2(totalWeightedArea)));
  Debug.Out(F("totalUnweightedP     %10s", LR2(totalUnweightedP)));
  Debug.Out(F("totalWeightedP       %10s", LR2(totalWeightedP)));
  Debug.Out(F("dynPPerUnitArea        %10s", LR4(dynPPerUnitArea)));

  FOR i := FIRST(blockP) TO LAST(blockP) DO
    WITH b = blocks[i] DO
      IF b.hasCustomP THEN
        blockP[i] := b.customP
      ELSE
        blockP[i] := b.weightedArea / totalWeightedArea * totalWeightedP
      END;
      Debug.Out(F("block %-15s blockP %8s",
                  b.name,
                  LR2(blockP[i])))
    END
  END;

  Debug.Out("====================  END BASELINE SETUP  ====================");

  (* initialize RedSizes *)
  RedSizes := BaseSizes;
  RedMauSizes := BaseSizes;

  RedSizes[FindBlock("MauPipes")]      := 18.0d0;
  RedMauSizes[FindBlock("MauPipes")]      := 18.0d0;
  
  RedSizes[FindBlock("PacketStorage")] := 64.0d0;

  DoScenario("BASELINE",             BaseClks    , BaseVs     , BaseSizes);
  DoScenario("ARCH SLOW",            ArchSlowClks, BaseVs     , BaseSizes);
  DoScenario("ARCH SLOW -50mV",      ArchSlowClks, Reduced50Vs, BaseSizes);
  DoScenario("ARCH SLOW -50mV PpsLOW",      ArchSlowClks, PpsLowV, BaseSizes);
  DoScenario("ARCH RED SLOW -50mV",  ArchSlowClks, Reduced50Vs, RedSizes);
  DoScenario("ARCH REDMAU SLOW -50mV",  ArchSlowClks, Reduced50Vs, RedMauSizes);
  DoScenario("ARCH REDMAU SLOW MauLOW", ArchSlowClks, PpsLowV    , RedMauSizes);
  DoScenario("ARCH REDMAU SLOW TmPpsLOW", ArchSlowClks, TmPpsLowV    , RedMauSizes);
  DoScenario("ARCH SLOW TmPpsLOW Pps1", ArchSlowPps1Clks, TmPpsLowV    , BaseSizes);
  DoScenario("ARCH REDMAU SLOW TmPpsLOW Pps1", ArchSlowPps1Clks, TmPpsLowV    , RedMauSizes);

  Debug.Out("====================  END DETAIL RUNS  ====================");

  DoSimpleModel()
END CbPower.
