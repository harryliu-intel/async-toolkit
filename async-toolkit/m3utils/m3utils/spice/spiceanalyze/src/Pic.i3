INTERFACE Pic;
IMPORT Refany;
IMPORT Canvas;

TYPE
  T <: Public;

  Extent = RECORD w, h : CARDINAL END;
  
  Public = OBJECT METHODS
    computeMinExtent() : Extent;
    render(to : Extent; canvas : Canvas.T);
  END;

CONST Brand = "Pic";

CONST Equal = Refany.Equal;
      
END Pic.
