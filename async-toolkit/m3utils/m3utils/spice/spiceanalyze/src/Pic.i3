INTERFACE Pic;
IMPORT Refany;

TYPE
  T <: Public;

  Extent = RECORD w, h : CARDINAL END;
  
  Public = OBJECT METHODS
    computeMinExtent() : Extent;
    render(to : Extent);
  END;

CONST Brand = "Pic";

CONST Equal = Refany.Equal;
      
END Pic.
