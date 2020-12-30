MODULE Main;

(* look at die cost of chopping Tofino Creek in two *)

IMPORT Math;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Wr, FileWr;
FROM YieldModel IMPORT Stapper;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT SchemeM3;
IMPORT SchemeStubs;
IMPORT ReadLine, SchemeReadLine;
IMPORT Debug;
IMPORT Scheme;
IMPORT Pathname;
IMPORT Thread;
IMPORT TextSeq;

<*FATAL Thread.Alerted*>

CONST LR = Fmt.LongReal;

PROCEDURE Stapper0_05(A, D0, n : LONGREAL) : LONGREAL =
  BEGIN RETURN Stapper(A, D0, n, 0.05d0) END Stapper0_05;
  
TYPE
  Chip = RECORD
    fixed, variable : LONGREAL;
    splitPerDie     : LONGREAL;
  END;


TYPE YieldFunction   = PROCEDURE(A : LONGREAL) : LONGREAL;
     RawCostFunction = PROCEDURE(A : LONGREAL) : LONGREAL;
     
TYPE
  Result = RECORD
    dice, fixedPerDie, varPerDie, splitPerDie, areaPerDie, costOfRawDie, yieldOfDie, costPerWorking, costPerProduct : LONGREAL;
  END;
  
PROCEDURE ProductSiCost(chip     : Chip;
                        yield    : YieldFunction;
                        rawCost  : RawCostFunction;
                        dieCount : [1..LAST(CARDINAL)] ) : Result =
  VAR
    dice := FLOAT(dieCount, LONGREAL);

    fixedPerDie := chip.fixed/dice;
    varPerDie   := chip.variable/dice;
    splitPerDie := ARRAY BOOLEAN OF LONGREAL
          { 0.0d0, chip.splitPerDie }[dieCount#1];

    areaPerDie  := fixedPerDie + varPerDie + splitPerDie;

    costOfRawDie := rawCost(areaPerDie);
    yieldOfDie   := yield  (areaPerDie);

    costPerWorking := costOfRawDie / yieldOfDie;
    costPerProduct := costPerWorking * dice;
  BEGIN
    RETURN Result { dice, fixedPerDie, varPerDie, splitPerDie, areaPerDie, costOfRawDie, yieldOfDie, costPerWorking, costPerProduct }
  END ProductSiCost;

PROCEDURE N5RawCost(A : LONGREAL) : LONGREAL =
  CONST
    DollarsPerWafer = 12451.0d0;
    (* this is the # for 2024Q1 N5_1p15M from Jeremy "best case" *)

    SqMmPerWafer = 404.0d0 * 142.0d0;
    (* Kishore's project has 142 dice of 404 sq mm each per wafer.

       This is just a guess!  A better value would come from knowing the
       actual X and Y of the dice we want and using a packing algorithm
       to pack the dice into the wafer, but we may also lose valuable
       insights from turning a "quiet" process into a "noisy" process
       in that way 
    *)

    DollarsPerSqMm = DollarsPerWafer / SqMmPerWafer;
  BEGIN
    RETURN A * DollarsPerSqMm
  END N5RawCost;

TYPE YieldModel = PROCEDURE(A, D0, n : LONGREAL) : LONGREAL;
     
PROCEDURE N5Yield(ym : YieldModel; A : LONGREAL) : LONGREAL =
  CONST
    D0 = 0.065d0;  (* 2024Q1 *)
    n  = 32.00d0;  (* complexity factor for N5_1P15M *)
  BEGIN
    RETURN ym(A, D0, n)
  END N5Yield;

CONST Reticle = 850.0d0; (* reticle size in sq mm *)
      Steps   = 100;

PROCEDURE DoIt(wr : Wr.T) =
  CONST
    TFc = Chip { fixed       := 150.0d0,
                 variable    := 0.0d0, (* will be overwritten *)
                 splitPerDie := 30.0d0 };
  VAR
    chip := TFc;
    result1, result2 : Result;

    Available := Reticle - chip.fixed;

  PROCEDURE MyYield(A : LONGREAL) : LONGREAL =
    BEGIN RETURN N5Yield(Stapper0_05, A) END MyYield;
    
  BEGIN
    FOR i := 1 TO Steps DO
      WITH A = Available / FLOAT(Steps, LONGREAL) * FLOAT(i, LONGREAL) DO
        chip.variable := A;

        result1 := ProductSiCost(chip, MyYield, N5RawCost, 1);
        result2 := ProductSiCost(chip, MyYield, N5RawCost, 2);

        Wr.PutText(wr,
                   F("%s %s %s\n",
                     LR(A),
                     LR(result1.costPerProduct),
                     LR(result2.costPerProduct)))
      END
    END
  END DoIt;

PROCEDURE GetPaths(extras : TextSeq.T) : REF ARRAY OF Pathname.T = 
  CONST
    fixed = ARRAY OF Pathname.T { "require", "m3" };
  VAR
    res := NEW(REF ARRAY OF Pathname.T, NUMBER(fixed) + extras.size());
  BEGIN
    FOR i := 0 TO NUMBER(fixed) - 1 DO
      res[i] := fixed[i]
    END;
    FOR i := NUMBER(fixed) TO extras.size() + NUMBER(fixed) - 1 DO
      res[i] := extras.remlo()
    END;
    RETURN res
  END GetPaths;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  doScheme := FALSE;
  extra := NEW(TextSeq.T).init();
BEGIN
  TRY
    doScheme := pp.keywordPresent("-scm");
    pp.skipParsed();
    WITH n = NUMBER(pp.arg^) - pp.next DO
      FOR i := 0 TO n - 1 DO
        extra.addhi(pp.getNext())
      END
    END;
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF doScheme THEN
    SchemeStubs.RegisterStubs();
    TRY
      WITH scm = NEW(SchemeM3.T).init(GetPaths(extra)^) DO
        SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    END
  ELSE
    WITH wr = FileWr.Open("tfc_12.dat") DO
      DoIt(wr);
      Wr.Close(wr)
    END
  END
END Main.
