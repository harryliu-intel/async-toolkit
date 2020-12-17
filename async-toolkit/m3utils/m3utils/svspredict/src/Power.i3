INTERFACE Power;
FROM SvsTypes IMPORT CornerData;

PROCEDURE Calc(READONLY dist : T; at : CornerData) : Result;

TYPE (* information about the distribution from which we are drawing *)
  T = RECORD
    RefP, FixedP, RefLeakP, LkgRatio, LkgRatioSigma : LONGREAL;
    Tt : CornerData;
  END;

  Result = RECORD
    cornerLkgRatio, leakPwr, totPwr : LONGREAL;
  END;
    
CONST Brand = "Power";
      
END Power.
