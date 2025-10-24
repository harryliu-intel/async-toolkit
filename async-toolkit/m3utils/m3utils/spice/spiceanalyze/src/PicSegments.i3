INTERFACE PicSegments;
IMPORT Pic;
IMPORT Refany;
IMPORT PicSegment;
IMPORT PicPoint;
IMPORT PicCircle;
IMPORT PicText;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init() : T;
    addSegment(READONLY seg  : PicSegment.T);
    addPoint  (READONLY at   : PicPoint.T);
    addCircle (READONLY circ : PicCircle.T);
    addText   (READONLY txt  : PicText.T);
  END;

CONST Brand = "PicSegments";

CONST Equal = Refany.Equal;

END PicSegments.
