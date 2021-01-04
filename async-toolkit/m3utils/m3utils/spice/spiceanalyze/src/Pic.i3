INTERFACE Pic;
IMPORT Refany;
IMPORT Canvas;
IMPORT PicCoord;

TYPE
  T <: Public;

  Extent = RECORD w, h : PicCoord.T END;
  
  Public = OBJECT METHODS
    init() : T;
    computeMinExtent() : Extent;
    render(at : PicCoord.T; to : Extent; canvas : Canvas.T);
  END;

CONST Brand = "Pic";

CONST Equal = Refany.Equal;
      
END Pic.
