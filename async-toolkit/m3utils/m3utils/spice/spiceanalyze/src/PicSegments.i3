INTERFACE PicSegments;
IMPORT Pic;
IMPORT Refany;
IMPORT PicSegment;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init() : T;
    addSegment(seg : PicSegment.T);
  END;

CONST Brand = "PicSegments";

CONST Equal = Refany.Equal;

END PicSegments.
