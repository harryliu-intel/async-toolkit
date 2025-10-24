INTERFACE SpeedSample;
IMPORT Transition;

TYPE
  T = RECORD
    speed      : CARDINAL;
    beg, end   : Transition.T;
    begT, endT : LONGREAL;
    cyc, freq  : LONGREAL;
    delcyc     : LONGREAL;

    pctFromPrev : LONGREAL; (* filled in later *)
  END;

CONST Brand = "SpeedSample";

PROCEDURE CompareBySpeed(READONLY a, b : T) : [-1..1];
  
PROCEDURE CompareByPctFromPrev(READONLY a, b : T) : [-1..1];

CONST Compare = CompareByPctFromPrev;

PROCEDURE Format(READONLY a : T) : TEXT;
      
END SpeedSample.
  
