
INTERFACE TimingPath;

TYPE
  T = RECORD
    startPoint : TEXT;
    startParen : TEXT;
    endPoint   : TEXT;
    endParen   : TEXT;
    scenario   : TEXT;
    pathGroup  : TEXT;
    pathType   : TEXT;
    transData  : TransPathData.T;
  END;

CONST Brand = "TimingPath";

END TimingPath.
