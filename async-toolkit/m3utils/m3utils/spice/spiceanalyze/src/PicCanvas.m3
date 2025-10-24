MODULE PicCanvas;

IMPORT PicSegments;
IMPORT PicPoint;
IMPORT PicCircle;
IMPORT PicText;
IMPORT PicSegment;

REVEAL
  T = Public BRANDED Brand OBJECT
    tgt : PicSegments.T;
  OVERRIDES
    init := Init;

    point := Point;
    circle := Circle;
    text := Text;
    segment := Segment;
  END;

PROCEDURE Init(t : T; tgt : PicSegments.T) : T =
  BEGIN
    t.tgt := tgt;
    RETURN t
  END Init;

PROCEDURE Point(t : T; READONLY p : PicPoint.T) =
  BEGIN t.tgt.addPoint(p) END Point;
  
PROCEDURE Circle(t : T; READONLY p : PicCircle.T) =
  BEGIN t.tgt.addCircle(p) END Circle;
  
PROCEDURE Text(t : T; READONLY p : PicText.T) =
  BEGIN t.tgt.addText(p) END Text;
  
PROCEDURE Segment(t : T; READONLY p : PicSegment.T) =
  BEGIN t.tgt.addSegment(p) END Segment;
  
BEGIN END PicCanvas.
