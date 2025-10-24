INTERFACE ArithProbability;
IMPORT ArithBits AS Bits;

TYPE
  T = RECORD
    lo, hi, count : Bits.Freq;
  END;

CONST Brand = "ArithProbability";

PROCEDURE Format(t : T) : TEXT;
      
END ArithProbability.
    
