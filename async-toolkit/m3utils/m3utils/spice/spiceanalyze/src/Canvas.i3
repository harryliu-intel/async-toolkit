INTERFACE Canvas;
IMPORT PicCoord;
IMPORT PicPoint, PicSegment;

TYPE
  T = OBJECT METHODS
    point(at : PicPoint.T);
    circle(at : PicPoint.T; radius : PicCoord.NonNeg);
    text(at : PicPoint.T; txt : Text);
    segment(at : PicSegment.T);
  END;

  Text = RECORD
    txt      : TEXT;
    size     : FontSize;
    width    : PicCoord.NonNeg;
    fontType : FontType;
  END;

  FontSize = CARDINAL;

  FontType = { Default, Serif, SansSerif };


CONST Brand = "Canvas";

END Canvas.
