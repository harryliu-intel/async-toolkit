INTERFACE DataPoint;

TYPE
  T = RECORD
    corner     : TEXT;
    V          : LONGREAL;
    temp       : LONGREAL;
    f          : LONGREAL;
  END;

CONST Brand = "DataPoint";

PROCEDURE CompareVoltage(READONLY a, b : T) : [-1..1];

CONST Compare = CompareVoltage;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END DataPoint.
