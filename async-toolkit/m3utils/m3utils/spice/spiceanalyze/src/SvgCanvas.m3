MODULE SvgCanvas;
IMPORT Wr;
IMPORT PicPoint, PicPointList;
IMPORT PicCircle, PicCircleList;
IMPORT PicText, PicTextList;
IMPORT PicSegment, PicSegmentList;

REVEAL
  T = Public BRANDED Brand OBJECT
    points   : PicPointList.T;
    circles  : PicCircleList.T;
    texts    : PicTextList.T;
    segments : PicSegmentList.T;
  OVERRIDES
    init  := Init;
    write := Write;
    
    point   := Point;
    circle  := Circle;
    text    := Text;
    segment := Segment;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.points := NIL;
    t.circles := NIL;
    t.texts := NIL;
    t.segments := NIL;
    RETURN t
  END Init;

PROCEDURE Write(t : T; to : Wr.T)
  RAISES { Wr.Failure } =
  BEGIN
  END Write;

PROCEDURE Point(t : T; point : PicPoint.T) =
  BEGIN
    t.points := PicPointList.Cons(point, t.points)
  END Point;

PROCEDURE Circle(t : T; circle : PicCircle.T) =
  BEGIN
    t.circles := PicCircleList.Cons(circle, t.circles)
  END Circle;

PROCEDURE Segment(t : T; segment : PicSegment.T) =
  BEGIN
    t.segments := PicSegmentList.Cons(segment, t.segments)
  END Segment;

PROCEDURE Text(t : T; text : PicText.T) =
  BEGIN
    t.texts := PicTextList.Cons(text, t.texts)
  END Text;

BEGIN END SvgCanvas.
  
