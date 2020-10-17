MODULE CbPower EXPORTS Main;

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
    name   : TEXT;
    clocks : CA;
    area   : REAL;
    weight : CARDINAL;
    hasCustomPower : BOOLEAN;
    customPower := 0.0d0;
    size := 1.0;
  END;
  
CONST
  Data = ARRAY OF BlockInfo {
  BlockInfo { name   := "Parser",
              clocks := CA { 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 13.59,
              weight := 4,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "Deparser",
              clocks := CA { 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 12.98,
              weight := 4,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "LearnEtc",
              clocks := CA { 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 7.99,
              weight := 4,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "MauPipes",
              clocks := CA { 0.0d0, 0.0d0, 1.0d0, 0.0d0, 0.0d0 },
              area   := 48.3,
              weight := 4,
              hasCustomPower := FALSE,
              size   := 20.0
  },
  BlockInfo { name   := "PipeTmIntf",
              clocks := CA { 0.5d0, 0.5d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 11.23,
              weight := 1,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "PacketStorage",
              clocks := CA { 0.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0 },
              area   := 62.8,
              weight := 1,
              hasCustomPower := FALSE,
              size   := 96.0
  },
  BlockInfo { name   := "QueueingLogic",
              clocks := CA { 0.0d0, 0.4d0, 0.6d0, 0.0d0, 0.0d0 },
              area   := 95.01,
              weight := 1,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "MacPcs",
              clocks := CA { 0.0d0, 0.0d0, 0.0d0, 1.0d0, 0.0d0 },
              area   := 22.88,
              weight := 1,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "Misc",
              clocks :=   CA { 0.2d0, 0.2d0, 0.2d0, 0.2d0, 0.2d0 },
              area   := 56.49,
              weight := 1,
              hasCustomPower := FALSE
  },
  BlockInfo { name   := "Serdes",
              clocks :=    CA { 0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0 }, 
              area   := 235.0,
              weight := 1,
              hasCustomPower := TRUE,
              customPower := BaselineSerdesP
  }
  };

CONST
  BaselineP56G       = 503.0d0;
  BaselineP112Gvs56G = 25.0d0 * 1.5d0;
  BaselineP112G      = BaselineP56G - BaselineP112Gvs56G;
  BaselineLeakageP   = 30.0d0;
  BaselinePackBytes  = 247;
  BaselineV          = 0.75d0;
  BaselineSerdesEpb  = 7.5d-12;
  BaselineSerdesbps  = 12.8d12;
  BaselineSerdesP    = BaselineSerdesEpb * BaselineSerdesbps;
  
  BaselineClks       = ARRAY Clock OF LONGREAL {
  1.35d0,
  1.505d0,
  1.5d0,
  1.0d0,
  1.0d0
  };
  
BEGIN
  
END CbPower.
