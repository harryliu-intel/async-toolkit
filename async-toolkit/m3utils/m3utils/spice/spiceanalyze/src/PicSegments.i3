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
    addSegment(seg : PicSegment.T);
    addPoint(at : PicPoint.T);
    addCircle(circ : PicCircle.T);
    addText(txt : PicText.T);
END;

CONST Brand = "PicSegments";

CONST Equal = Refany.Equal;

END PicSegments.
