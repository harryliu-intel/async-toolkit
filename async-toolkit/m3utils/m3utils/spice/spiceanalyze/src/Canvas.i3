INTERFACE Canvas;
IMPORT PicPoint;
IMPORT PicCircle;
IMPORT PicText;
IMPORT PicSegment;

TYPE
  T = OBJECT METHODS
    point(at : PicPoint.T);
    circle(circ : PicCircle.T);
    text(txt : PicText.T);
    segment(at : PicSegment.T);
  END;

CONST Brand = "Canvas";

END Canvas.
