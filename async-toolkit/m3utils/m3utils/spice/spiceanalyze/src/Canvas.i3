INTERFACE Canvas;
IMPORT PicPoint;
IMPORT PicCircle;
IMPORT PicText;
IMPORT PicSegment;

TYPE
  T = OBJECT METHODS
    point  (READONLY p    : PicPoint.T);
    circle (READONLY circ : PicCircle.T);
    text   (READONLY txt  : PicText.T);
    segment(READONLY seg  : PicSegment.T);
  END;

CONST Brand = "Canvas";

END Canvas.
