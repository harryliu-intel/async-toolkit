MODULE PicSegments;

IMPORT PicPoint, PicPointList;
IMPORT PicCircle, PicCircleList;
IMPORT PicText, PicTextList;
IMPORT PicSegment, PicSegmentList;
IMPORT PicExtent;

REVEAL
  T = Public BRANDED Brand OBJECT
    extent    : PicExtent.T;
    points    : PicPointList.T;
    circles   : PicCircleList.T;
    texts     : PicTextList.T;
    segments  : PicSegmentList.T;
  OVERRIDES
    init := Init;
    addSegment := AddSegment;
    addPoint := AddPoint;
    addCircle := AddCircle;
    addText := AddText;
    minExtent := MinExtent;
  END;

PROCEDURE MinExtent(t : T) : PicExtent.T =
  BEGIN
    RETURN t.extent
  END MinExtent;

PROCEDURE Init(t : T) : T =
  BEGIN
    t.extent := PicExtent.Empty;
    t.points := NIL;
    t.circles := NIL;
    t.texts := NIL;
    t.segments := NIL;
    RETURN t
  END Init;

PROCEDURE AddSegment(t : T; segment : PicSegment.T) =
  BEGIN
    t.segments := PicSegmentList.Cons(segment, t.segments);
    t.extent := PicExtent.Merge(t.extent, PicSegment.Extent(segment))
  END AddSegment;

PROCEDURE AddPoint(t : T; point : PicPoint.T) =
  BEGIN
    t.points := PicPointList.Cons(point, t.points);
    t.extent := PicExtent.Merge(t.extent, PicPoint.Extent(point))
  END AddPoint;

PROCEDURE AddCircle(t : T; circle : PicCircle.T) =
  BEGIN
    t.circles := PicCircleList.Cons(circle, t.circles);
    t.extent := PicExtent.Merge(t.extent, PicCircle.Extent(circle))
  END AddCircle;

PROCEDURE AddText(t : T; text : PicText.T) =
  BEGIN
    t.texts := PicTextList.Cons(text, t.texts);
    t.extent := PicExtent.Merge(t.extent, PicText.Extent(text))
  END AddText;

BEGIN END PicSegments.
