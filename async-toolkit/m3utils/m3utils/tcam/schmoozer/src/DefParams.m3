MODULE DefParams;
FROM Schmoozer IMPORT RealParam;

BEGIN

  vddP    := NEW(RealParam,
                 nm := "vdd",
                 flag := "vdd",
                 saneMin := 0.20d0, saneMax := 2.0d0).init();

  tempP   := NEW(RealParam,
                 nm := "temp",
                 flag := "temp",
                 saneMin := -100.0d0, saneMax := 200.0d0).init();

  clkP    := NEW(RealParam,
                 nm := "clk",
                 flag := "clk",
                 saneMin := 100.0d6, saneMax := 20.0d9).init();

  holdP   := NEW(RealParam,
                 nm := "holdfrac",
                 flag := "holdfrac",
                 saneMin := 0.0d0, saneMax := 1.0d0).init();

END DefParams.
