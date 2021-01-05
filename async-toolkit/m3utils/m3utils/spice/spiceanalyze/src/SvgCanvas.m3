MODULE SvgCanvas;
IMPORT Wr;
IMPORT PicPoint, PicPointList;
IMPORT PicCircle, PicCircleList;
IMPORT PicText, PicTextList;
IMPORT PicSegment, PicSegmentList;
IMPORT SvgColor;
FROM Fmt IMPORT Int, LongReal, F, FN;
IMPORT PicCoord;
IMPORT Thread;

(* this is basically a duplicate of PicSegments.m3 ??? *)

REVEAL
  T = Public BRANDED Brand OBJECT
    points    : PicPointList.T;
    circles   : PicCircleList.T;
    texts     : PicTextList.T;
    segments  : PicSegmentList.T;

    defColor  : SvgColor.T;
    defStrokeWidth : PicCoord.T;
    
  OVERRIDES
    init  := Init;
    write := Write;
    
    point   := Point;
    circle  := Circle;
    text    := Text;
    segment := Segment;
  END;

PROCEDURE Init(t : T; defColor : SvgColor.T; defStrokeWidth : PicCoord.T) : T =
  BEGIN
    t.points := NIL;
    t.circles := NIL;
    t.texts := NIL;
    t.segments := NIL;
    t.defColor := defColor;
    t.defStrokeWidth := defStrokeWidth;
    RETURN t
  END Init;

PROCEDURE Write(t : T; wr : Wr.T)
  RAISES { Wr.Failure, Thread.Alerted } =

  PROCEDURE N(z : PicCoord.T) : TEXT =
    BEGIN
      RETURN F("\"%s\"", LongReal(z))
    END N;

  PROCEDURE I(i : INTEGER) : TEXT =
    BEGIN
      RETURN F("\"%s\"", Int(i))
    END I;
    
  VAR
    dct    := SvgColor.Format(t.defColor);
    stroke := t.defStrokeWidth;
  BEGIN
    Wr.PutText(wr,
               "<svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\">\n");

    VAR
      p := t.points;
    BEGIN
      WHILE p # NIL DO
        Wr.PutText(wr,
                   FN("<circle cx=%s cy=%s r=%s stroke-width=%s stroke=\"%s\" fill=\"%s\" />\n\n",
                      ARRAY OF TEXT {
        N(p.head.x),
        N(p.head.y),
        N(stroke / 2.0d0),
        N(stroke),
        dct,
        dct}));
        p := p.tail
      END
    END;

    VAR
      p := t.circles;
    BEGIN
      WHILE p # NIL DO
        Wr.PutText(wr,
                   F("<circle cx=%s cy=%s r=%s stroke-width=%s stroke=\"%s\" />\n\n",
                     N(p.head.at.x),
                     N(p.head.at.y),
                     N(p.head.r),
                     N(stroke),
                     dct));
        p := p.tail
      END
    END;

    VAR
      p := t.texts;
    BEGIN
      WHILE p # NIL DO
        Wr.PutText(wr,
                   F(" <text x=%s y=%s font-size=%s text-anchor=\"middle\" fill=\"%s\">%s</text>\n\n",
                     N(p.head.ll.x),
                     N(p.head.ll.y),
                     N(p.head.size),
                     dct,
                     p.head.txt)
        );
        p := p.tail
      END
    END;


    VAR
      p := t.segments;
    BEGIN
      WHILE p # NIL DO
        Wr.PutText(wr,
                   FN("<line x1=%s y1=%s x2=%s y2=%s stroke-width=%s stroke=\"%s\" />\n\n",
                      ARRAY OF TEXT {
        N(p.head.a.x),
        N(p.head.a.y),
        N(p.head.b.x),
        N(p.head.b.y),
        N(stroke),
        dct
        }
        ));
        p := p.tail
      END
    END;

    Wr.PutText(wr, "</svg>\n")
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
  
