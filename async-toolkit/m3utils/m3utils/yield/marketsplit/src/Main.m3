MODULE Main;
IMPORT IO;
FROM Fmt IMPORT F, LongReal, Int;

CONST LR = LongReal;
      
TYPE
  Die    = { FullSwitch, HalfSwitch, SwitchCore, IoChiplet };

  Yields = ARRAY OF LONGREAL;

  Table  = ARRAY Die OF DieData;

  DieData = RECORD
    area     : LONGREAL;
    binNames : REF ARRAY OF TEXT;
    bins     : REF Yields;
    upBins   : REF ARRAY OF REF ARRAY OF TEXT;
  END;

  TA = ARRAY OF TEXT;

  TRA = ARRAY OF REF ARRAY OF TEXT;

  RAT = REF ARRAY OF TEXT;

CONST
  DieNames = ARRAY Die OF TEXT { "FullSwitch",
                                 "HalfSwitch",
                                 "SwitchCore",
                                 "IoChiplet" };

VAR
  Data := Table {  DieData { 820.0d0,
                             MkT ( "top", "half", "sixteenT" ),
                             MkRef( Yields { 33.0d0, 17.0d0, 26.0d0 }),
                             MkR ( MkT(), MkT("top"), MkT("top", "half") ) },
                   DieData { 455.0d0,
                             MkT ( "top", "evenodd", "sixteenT" ),
                             MkRef( Yields { 50.0d0, 31.0d0,  3.0d0 }),
                             MkR ( MkT(), MkT("top"), MkT("top") ) },
                   DieData { 615.0d0,
                             MkT ( "top", "half", "sixteenT" ),
                             MkRef( Yields { 45.0d0, 17.0d0, 26.0d0 }),
                             MkR ( MkT(), MkT("top"), MkT("top", "half") ) },
                   DieData { 155.0d0,
                             MkT ( "top" ),
                             MkRef( Yields { 75.0d0 }),
                             MkR ( MkT() )  }
  } ;

TYPE
  Approach = { A1Die, A2Die, A5Die };

  Sku      = { S51T, S25T, S16T };

  BinCount = RECORD
    die : Die;
    bin : TEXT;
    cnt : CARDINAL;
  END;

  BA = ARRAY OF BinCount;
  
  Assembly = BA;

  ApproachData = ARRAY Sku OF REF Assembly;

  Mix = ARRAY Sku OF LONGREAL;
  
  Market = RECORD
    name : TEXT;
    mix  : Mix;
  END;

CONST
  SkuNames = ARRAY Sku OF TEXT { "51T", "25T", "16T" };
  ApproachNames = ARRAY Approach OF TEXT { "1-die", "2-die", "5-die" };
  
CONST
  LowMarket  = Market { "low",  Mix { 0.30d0, 0.30d0, 0.40d0 } };
  BaseMarket = Market { "base", Mix { 0.50d0, 0.35d0, 0.15d0 } };
  HighMarket = Market { "high", Mix { 0.65d0, 0.35d0, 0.00d0 } };
  ExtrMarket = Market { "extr", Mix { 1.00d0, 0.00d0, 0.00d0 } };

  Markets = ARRAY OF Market { LowMarket,
                              BaseMarket,
                              HighMarket,
                              ExtrMarket };

VAR
  A1Approach := ApproachData {
  MkAss( BA { BinCount { Die.FullSwitch,      "top", 1 } } ),
  MkAss( BA { BinCount { Die.FullSwitch,     "half", 1 } } ),
  MkAss( BA { BinCount { Die.FullSwitch, "sixteenT", 1 } } )
  };

  A2Approach := ApproachData {
  MkAss( BA{ BinCount { Die.HalfSwitch,      "top", 2 } } ),
  MkAss( BA{ BinCount { Die.HalfSwitch,  "evenodd", 2 } } ),
  MkAss( BA{ BinCount { Die.HalfSwitch, "sixteenT", 1 } } )
  };

  A5Approach := ApproachData {
  MkAss( BA { BinCount { Die.SwitchCore,      "top", 1 },
              BinCount { Die.IoChiplet,       "top", 4 } } ),
  MkAss( BA { BinCount { Die.SwitchCore,     "half", 1 },
              BinCount { Die.IoChiplet,       "top", 4 }  } ),
  MkAss( BA { BinCount { Die.SwitchCore, "sixteenT", 1 },
              BinCount { Die.IoChiplet,       "top", 4 }  } )
  };

  AllApproaches :=
      ARRAY Approach OF ApproachData { A1Approach, A2Approach, A5Approach };
  
CONST
  Build = 10000;
  
PROCEDURE MkRef(READONLY a : ARRAY OF LONGREAL) : REF ARRAY OF LONGREAL =
  VAR
    res := NEW(REF ARRAY OF LONGREAL, NUMBER(a));
  BEGIN
    res^ := a;
    RETURN res
  END MkRef;

PROCEDURE MkAss(READONLY b : BA) : REF BA =
  VAR
    res := NEW(REF BA, NUMBER(b));
  BEGIN
    res^ := b;
    RETURN res
  END MkAss;

PROCEDURE MkT(a0, a1, a2, a3, a4 : TEXT := NIL) : REF ARRAY OF TEXT =
  TYPE
    a = ARRAY OF TEXT;
    A = REF a;
  VAR
    res : A;
  BEGIN
    IF    a4 # NIL THEN
      res := NEW(A, 5);
      res^ := a { a0, a1, a2, a3, a4 }
    ELSIF a3 # NIL THEN
      res := NEW(A, 4);
      res^ := a { a0, a1, a2, a3 }
    ELSIF a2 # NIL THEN
      res := NEW(A, 3);
      res^ := a { a0, a1, a2 }
    ELSIF a1 # NIL THEN
      res := NEW(A, 2);
      res^ := a { a0, a1 }
    ELSIF a0 # NIL THEN
      res := NEW(A, 1);
      res^ := a { a0 }
    ELSE
      res := NEW(A, 0)
    END;
    RETURN res
  END MkT;

PROCEDURE MkR(a0, a1, a2, a3, a4 : RAT := NIL) : REF ARRAY OF RAT =
  TYPE
    a = ARRAY OF RAT;
    A = REF a;
  VAR
    res : A;
  BEGIN
    IF    a4 # NIL THEN
      res := NEW(A, 5);
      res^ := a { a0, a1, a2, a3, a4 }
    ELSIF a3 # NIL THEN
      res := NEW(A, 4);
      res^ := a { a0, a1, a2, a3 }
    ELSIF a2 # NIL THEN
      res := NEW(A, 3);
      res^ := a { a0, a1, a2 }
    ELSIF a1 # NIL THEN
      res := NEW(A, 2);
      res^ := a { a0, a1 }
    ELSIF a0 # NIL THEN
      res := NEW(A, 1);
      res^ := a { a0 }
    ELSE
      res := NEW(A, 0)
    END;
    RETURN res
  END MkR;

PROCEDURE DoMarket(mkt : Market) =
  VAR
    target : ARRAY Sku OF CARDINAL;
    sumW := 0.0d0;
  BEGIN
    IO.Put(F("market %s\n", mkt.name));
    FOR s := FIRST(Sku) TO LAST(Sku) DO
      sumW := sumW + mkt.mix[s];
    END;
    FOR s := FIRST(Sku) TO LAST(Sku) DO
      target[s] := ROUND(FLOAT(Build, LONGREAL) * mkt.mix[s] / sumW);
      IO.Put(F("sku %s proportion %s count %s\n",
               SkuNames[s],
               LR(mkt.mix[s]),
               Int(target[s])));
    END;
    FOR a := FIRST(AllApproaches) TO LAST(AllApproaches) DO
      DoApproach(mkt, target, ApproachNames[a], AllApproaches[a])
    END
  END DoMarket;

PROCEDURE DoApproach(READONLY mkt    : Market;
                     READONLY target : ARRAY Sku OF CARDINAL;
                     appName         : TEXT;
                     READONLY app    : ApproachData) =
  VAR
    needed := ApproachDieNeeded(app);
  BEGIN
    IO.Put(F("approach %s\n", appName));
  END DoApproach;

PROCEDURE ApproachDieNeeded(READONLY app : ApproachData) : SET OF Die =
  VAR
    needed := SET OF Die {};
  BEGIN
    (* find die types needed *)
    FOR s := FIRST(Sku) TO LAST(Sku) DO
      WITH ass = app[s]^ DO
        FOR k := FIRST(ass) TO LAST(ass) DO
          needed := needed + SET OF Die { ass[k].die }
        END
      END
    END;
    RETURN needed
  END ApproachDieNeeded;
  
BEGIN
  FOR m := FIRST(Markets) TO LAST(Markets) DO
    DoMarket(Markets[m])
  END
END Main.

  
