INTERFACE LatchSpecs;
FROM SpiceTiming IMPORT Arc, ArcArr, LatchSpec, N, UD, Up, Dn,  NoArc;
IMPORT CheckMode;

(*
 
Per Andrew 11/9/2022:

The regular/vendor latch:

setup margin is:
  min time of any change in D to rising CLK

hold margin is:
  min time of falling CLK to any change in D

Your TINY_LATCH:

setup margin is:
  min of
    min time of rising D to rising CLK
    min time of falling D to falling _CLK

hold margin is:
  min of
    min time of falling CLK to rising D
    min time of rising _CLK to falling D

... see below for more detailed timing diagrams ...

*)

CONST
  VendorArcs = 
    ArcArr {
    Arc { N { "d", FALSE },   N { "clk", FALSE }, CheckMode.T.Setu,  1, UD },
    Arc { N { "d", FALSE },   N { "clk", FALSE }, CheckMode.T.Hold,  1, UD },
    Arc { N { "d", FALSE },   N { "clk", FALSE }, CheckMode.T.Hold,  1, UD },

    (* pulse width arcs are relative to the nc1 inverted internal clock *)
    Arc { N { "nk2", TRUE }, N { "clk", FALSE }, CheckMode.T.Puls,  1, Up },
    Arc { N { "nk2", TRUE }, N { "nc1", TRUE  }, CheckMode.T.Puls, -1, Dn },
    NoArc,
    ..
  };
  
  VendorLatch = LatchSpec { "vendor_D_intel_D_g1i_D_l", VendorArcs };
  VendorLatch2 = LatchSpec { "g1ilsn000aa2n04x5", VendorArcs };

  TinyArcs =   ArcArr {
    Arc { N { "D",  FALSE }, N { "CLK"   , FALSE }, CheckMode.T.Setu,  1, Up },
    Arc { N { "D",  FALSE }, N { "_CLK", FALSE }, CheckMode.T.Setu, -1, Dn },
    Arc { N { "D",  FALSE }, N { "CLK"   , FALSE }, CheckMode.T.Hold,  1, Up },
    Arc { N { "D",  FALSE }, N { "_CLK", FALSE }, CheckMode.T.Hold, -1, Dn },
    Arc { N { "nk", TRUE  }, N { "CLK"   , FALSE }, CheckMode.T.Puls,  1, Up },
    Arc { N { "nk", TRUE  }, N { "_CLK", FALSE }, CheckMode.T.Puls, -1, Dn }
  };
    
  TinyLatch = LatchSpec { "D_TINY_U_LATCH_D_a.v.", TinyArcs };
  TinyLatch2 = LatchSpec { "g1iltny00aa2n01x5", TinyArcs };
  TinyLatch3 = LatchSpec { "g1iltny00aa2n02x5", TinyArcs };
  
  Specs = ARRAY OF LatchSpec { VendorLatch,
                                    VendorLatch2,
                                    TinyLatch,
                                    TinyLatch2,
                                    TinyLatch3
                                    };

END LatchSpecs.
