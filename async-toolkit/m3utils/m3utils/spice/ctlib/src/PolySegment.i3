INTERFACE PolySegment;
IMPORT LRRegression AS Regression;

TYPE
  T = RECORD
    r  : Regression.T;
    lo : INTEGER;
    n  : CARDINAL;
  END;

CONST Brand = "PolySegment";

END PolySegment.
