(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SvgCanvas;
IMPORT Wr;
IMPORT PicPoint, PicPointList;
IMPORT PicCircle, PicCircleList;
IMPORT PicText, PicTextList;
IMPORT PicSegment, PicSegmentList;
IMPORT SvgColor;
FROM Fmt IMPORT LongReal, F, FN;
IMPORT PicCoord;
IMPORT Thread;
IMPORT Debug;
IMPORT PicExtent;

(* this is basically a duplicate of PicSegments.m3 ??? *)

CONST doDebug = TRUE;
      LR = LongReal;
      
REVEAL
  T = Public BRANDED Brand OBJECT
    points    : PicPointList.T;
    circles   : PicCircleList.T;
    texts     : PicTextList.T;
    segments  : PicSegmentList.T;

    defColor  : SvgColor.T;
    defStrokeWidth : PicCoord.T;

    extent    : PicExtent.T;
    
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
    t.extent := PicExtent.Empty;
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

  PROCEDURE FormatViewBox() : TEXT =
    CONST
      Pad = 0.2d0;
    VAR
      xs := t.extent.ur.x - t.extent.ll.x;
      ys := t.extent.ur.y - t.extent.ll.y;
    BEGIN
      IF t.extent = PicExtent.Empty THEN RETURN "0 0 0 0" END;
      
      (* we are going to flip the signs on the Y-extent *)
      (* -ur.y is therefore the min Y coord in the image *)
      RETURN F("%s %s %s %s",
               LR( t.extent.ll.x - Pad * xs),     (* origin X *)
               LR(-t.extent.ur.y - Pad * ys),     (* origin Y *)
               LR(xs * (1.0d0 + 2.0d0 * Pad)),
               LR(ys * (1.0d0 + 2.0d0 * Pad)));
    END FormatViewBox;
    
  VAR
    dct    := SvgColor.Format(t.defColor);
    wht    := SvgColor.Format(SvgColor.White);
    stroke := t.defStrokeWidth;
  BEGIN
    Wr.PutText(wr,
               F("<svg version=\"1.1\" baseProfile=\"full\" viewBox=\"%s\" xmlns=\"http://www.w3.org/2000/svg\">\n",
                 FormatViewBox()));

    VAR
      p := t.points;
    BEGIN
      WHILE p # NIL DO
        Wr.PutText(wr,
                   FN("<circle cx=%s cy=%s r=%s stroke-width=%s stroke=\"%s\" fill=\"%s\" />\n\n",
                      ARRAY OF TEXT {
        N(p.head.x),
        N(-p.head.y),
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
                   FN("<circle cx=%s cy=%s r=%s stroke-width=%s stroke=\"%s\" fill=\"%s\"/>\n\n",
                      ARRAY OF TEXT {
        N(p.head.at.x),
        N(-p.head.at.y),
        N(p.head.r),
        N(stroke),
        dct,
        wht
        } ));
        
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
                     N(-p.head.ll.y),
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
        N(-p.head.a.y),
        N(p.head.b.x),
        N(-p.head.b.y),
        N(stroke),
        dct
        }
        ));
        p := p.tail
      END
    END;

    Wr.PutText(wr, "</svg>\n")
  END Write;

PROCEDURE Point(t : T; READONLY point : PicPoint.T) =
  BEGIN
    IF doDebug THEN
      Debug.Out("SvgCanvas : point " & PicPoint.Format(point))
    END;
    t.extent := PicExtent.Merge(t.extent, PicPoint.Extent(point));
    t.points := PicPointList.Cons(point, t.points)
  END Point;

PROCEDURE Circle(t : T; READONLY circle : PicCircle.T) =
  BEGIN
    IF doDebug THEN
      Debug.Out("SvgCanvas : circle " & PicCircle.Format(circle))
    END;
    t.extent  := PicExtent.Merge(t.extent, PicCircle.Extent(circle));
    t.circles := PicCircleList.Cons(circle, t.circles)
  END Circle;

PROCEDURE Segment(t : T; READONLY segment : PicSegment.T) =
  BEGIN
    IF doDebug THEN
      Debug.Out("SvgCanvas : segment " & PicSegment.Format(segment))
    END;
    t.extent   := PicExtent.Merge(t.extent, PicSegment.Extent(segment));
    t.segments := PicSegmentList.Cons(segment, t.segments)
  END Segment;

PROCEDURE Text(t : T; READONLY text : PicText.T) =
  BEGIN
    IF doDebug THEN
      Debug.Out("SvgCanvas : text " & PicText.Format(text))
    END;
    t.extent := PicExtent.Merge(t.extent, PicText.Extent(text));
    t.texts := PicTextList.Cons(text, t.texts)
  END Text;

BEGIN END SvgCanvas.
  
