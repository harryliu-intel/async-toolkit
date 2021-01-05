INTERFACE Power;
FROM SvsTypes IMPORT CornerData;
IMPORT Corner;

PROCEDURE Calc(READONLY dist : Params;
               READONLY at   : CornerData) : Result;

TYPE (* information about the distribution from which we are drawing *)
  Params = RECORD
    (* these here are values that pertain to a TT chip atthe operating 
       conditions where we are drawing our samples.

       For example, if we are interested in evaluating a product with
       a clock speed of 1.2 GHz at 105C, and what we have is measurements
       pertaining to one sample from that product at 1.0 GHz at 75C and
       known place in the distribution of the fab process, we need to
       pre-scale the known parameters of the sample to the TT of the
       fab process at the target temperature and voltage for the product *)

    RefP, FixedP, RefLeakP, LkgRatio, LkgRatioSigma : LONGREAL;

    c : ARRAY Corner.T OF CornerData;
    (* data for the timing and power voltages across the corners of the
       process with given sigma values for the points in the distribution *)
    
  END;

  Result = RECORD
    cornerLkgRatio, leakPwr, totPwr : LONGREAL;
  END;

PROCEDURE FmtParams(READONLY p : Params) : TEXT;
  
CONST Brand = "Power";
      
END Power.
