(* $Id: ANOVAProgramBits.i3,v 1.3 2010/04/22 08:28:55 mika Exp $ *)

INTERFACE ANOVAProgramBits;
IMPORT FactorialDesign, FactorialDatumList;
IMPORT ANOVA;

TYPE
  Mode = { FactorialANOVA, AllAverages, AllDevs, MinMax, Confidence, AllValues };

TYPE 
  F <: PubF;

  PubF = FactorialDesign.T OBJECT
    list : FactorialDatumList.T := NIL;
  END;

TYPE Sortby = { Min, Mean, Max, None };

CONST Brand = "ANOVAProgramBits";

PROCEDURE DoFactorialANOVAMode(anova : ANOVA.T;
                               rx : TEXT;
                               anaHighestAve : BOOLEAN;
                               d : F);

PROCEDURE DoOtherMode(mode : Mode; 
                      anova : ANOVA.T; 
                      rx : TEXT; 
                      d : F; 
                      sortBy : Sortby);

PROCEDURE SetNoRound();

END ANOVAProgramBits.
