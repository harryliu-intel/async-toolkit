INTERFACE HspVariation;
IMPORT Variation;

TYPE
  T <: Public;

  Public = Variation.T OBJECT METHODS
    getVars(fromLib, baseTranTypeName : TEXT) : REF ARRAY OF TEXT;
  END;
  
CONST Brand = "HspVariation";

CONST GeoVars = ARRAY OF TEXT { "M", "Ldrawn", "Wdrawn", "NF", "scale" };

CONST FixedParams = ARRAY OF TEXT { "widtnflag" };
      
END HspVariation.
