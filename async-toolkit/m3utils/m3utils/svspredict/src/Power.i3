INTERFACE Power;
FROM SvsTypes IMPORT CornerData;
IMPORT Corner;

PROCEDURE Calc(READONLY dist : Params; at : CornerData) : Result;

TYPE (* information about the distribution from which we are drawing *)
  Params = RECORD
    RefP, FixedP, RefLeakP, LkgRatio, LkgRatioSigma : LONGREAL;
    c : ARRAY Corner.T OF CornerData;
  END;

  Result = RECORD
    cornerLkgRatio, leakPwr, totPwr : LONGREAL;
  END;
    
CONST Brand = "Power";
      
END Power.
