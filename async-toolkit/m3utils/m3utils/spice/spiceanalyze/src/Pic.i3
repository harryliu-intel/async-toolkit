INTERFACE Pic;
IMPORT Refany;
IMPORT Canvas;
IMPORT PicCoord;
IMPORT PicExtent;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;

    minExtent() : PicExtent.T;
    (* extent of content, bottom up *)

    setExtent(READONLY to : PicExtent.T);
    (* extent to render, top down *)
    
    curExtent() : PicExtent.T;
    (* current extent to render *)
    
    render(READONLY at : PicCoord.T; canvas : Canvas.T);
    (* request to render *)
  END;

CONST Brand = "Pic";

CONST Equal = Refany.Equal;
      
END Pic.
