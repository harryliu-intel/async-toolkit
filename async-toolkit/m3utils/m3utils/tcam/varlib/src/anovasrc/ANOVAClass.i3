(* $Id: ANOVAClass.i3,v 1.1 2006/03/06 02:29:02 mika Exp $ *)

INTERFACE ANOVAClass;
IMPORT ANOVA, ANOVASuper;
FROM ANOVA IMPORT Order;

TYPE RarrL = REF ARRAY OF LONGREAL;
     RarrO = REF ARRAY OF Order;
     RarrC = REF ARRAY OF CARDINAL;

TYPE
  Private = ANOVA.Public OBJECT METHODS
    mainEffCoeffs(varIdx : CARDINAL; 
                  order : CARDINAL) : RarrL;
    valueEffCoeffs(varIdx : CARDINAL; 
                   value : CARDINAL) : RarrL;
    interactionCoeffs(READONLY orders : ARRAY OF Order) : RarrL;
  END;

REVEAL ANOVASuper.T <: Private;

PROCEDURE UnitVector(sz : CARDINAL) : RarrL;
PROCEDURE Normalize(VAR a : ARRAY OF LONGREAL);
PROCEDURE Dot(READONLY a, b : ARRAY OF LONGREAL) : LONGREAL;
PROCEDURE ComponentMul(READONLY a, b : ARRAY OF LONGREAL;
                       VAR r : ARRAY OF LONGREAL;
                       scalar := 1.0d0);
END ANOVAClass.
