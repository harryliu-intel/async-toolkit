MODULE PicSegments;
IMPORT PicSegmentList;
IMPORT PicSegment;

REVEAL
  T = Public BRANDED Brand OBJECT
    segs : PicSegmentList.T;
  OVERRIDES
    init := Init;
    addSegment := AddSegment;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN t.segs := NIL; RETURN t END Init;

PROCEDURE AddSegment(t : T; seg : PicSegment.T) =
  BEGIN t.segs := PicSegmentList.Cons(seg, t.segs) END AddSegment;

BEGIN END PicSegments.
