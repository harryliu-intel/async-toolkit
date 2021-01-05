MODULE PicExtent;
IMPORT PicPoint;

PROCEDURE Merge(READONLY a, b : T) : T =
  VAR
    ll := PicPoint.T { MIN(a.ll.x, b.ll.x), MIN(a.ll.y, b.ll.y) };
    ur := PicPoint.T { MIN(a.ur.x, b.ur.x), MIN(a.ur.y, b.ur.y) };

  BEGIN
    RETURN T { ll, ur }
  END Merge;

BEGIN END PicExtent.
