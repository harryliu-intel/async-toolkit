MODULE DrawElements;

IMPORT PicPoint;
IMPORT PicCoord;
IMPORT PicSegment;
IMPORT PicCircle;

IMPORT PicSegments;

CONST Grid  = 5.0d0;
      Small = 3.0d0;
     
PROCEDURE DrawFet(to     : PicSegments.T;
                  type   : FetType;
                  doBody : BOOLEAN) =
  BEGIN
  END DrawFet;

PROCEDURE DrawRes(to : PicSegments.T) =
  BEGIN
  END DrawRes;

PROCEDURE DrawDio(to : PicSegments.T) =
  BEGIN
  END DrawDio;

PROCEDURE DrawCap(to : PicSegments.T) =
  BEGIN
  END DrawCap;

PROCEDURE DrawVdd(to : PicSegments.T) =
  BEGIN
  END DrawVdd;
    
PROCEDURE DrawGnd(to : PicSegments.T) =
  BEGIN
  END DrawGnd;


  (**********************************************************************)

PROCEDURE DrawRect(to   : PicSegments.T;
                   at   : PicPoint.T;
                   w, h : PicCoord.T) =
  VAR
    ll := PicPoint.Minus(at, PicPoint.T { -w / 2.0d0, 0.0d0 });
    ur := PicPoint.Plus (at, PicPoint.T { w, h });
    lr := PicPoint.T { ur.x, ll.y };
    ul := PicPoint.T { ur.y, ll.x };
  BEGIN
    AddClosedPath(to, ARRAY OF PicPoint.T { ll, lr, ur, ul });
  END DrawRect;


PROCEDURE AddClosedPath(to           : PicSegments.T;
                        READONLY lst : ARRAY OF PicPoint.T) =
  BEGIN
    WITH n = NUMBER(lst) DO
      FOR i := FIRST(lst) TO LAST(lst) DO
        to.addSegment( PicSegment.T { lst[i], lst[(i + 1) MOD n] } )
      END
    END
  END AddClosedPath;

PROCEDURE AddOpenPath(to           : PicSegments.T;
                      READONLY lst : ARRAY OF PicPoint.T) =
  BEGIN
    FOR i := FIRST(lst) TO LAST(lst) - 1 DO
      to.addSegment(PicSegment.T { lst[i], lst[i + 1] })
    END
  END AddOpenPath;

PROCEDURE DrawTriangle(to      : PicSegments.T;
                       point   : PicPoint.T;
                       h, w    : PicCoord.T;
                       pointUp : BOOLEAN) =
  VAR
    scale := 1.0d0;

    ul := PicPoint.Plus(point, PicPoint.T{ w / 2.0d0, h});
    ur := PicPoint.Plus(point, PicPoint.T{-w / 2.0d0, h});

    a := ARRAY [0 .. 2] OF PicPoint.T { point, ul, ur };
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF pointUp THEN
        a[i] := PicPoint.Times(-1.0d0, a[i])
      END
    END;

    AddClosedPath(to, a)
  END DrawTriangle;

BEGIN END DrawElements.
