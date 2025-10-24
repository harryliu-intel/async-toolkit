(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE PicSegments;

IMPORT PicPoint, PicPointList;
IMPORT PicCircle, PicCircleList;
IMPORT PicText, PicTextList;
IMPORT PicSegment, PicSegmentList;
IMPORT PicExtent;
IMPORT Canvas;
IMPORT Debug;

CONST doDebug = TRUE;

REVEAL
  T = Public BRANDED Brand OBJECT
    extent    : PicExtent.T;
    points    : PicPointList.T;
    circles   : PicCircleList.T;
    texts     : PicTextList.T;
    segments  : PicSegmentList.T;
  OVERRIDES
    init       := Init;
    addSegment := AddSegment;
    addPoint   := AddPoint;
    addCircle  := AddCircle;
    addText    := AddText;

    minExtent  := MinExtent;
    render     := Render;
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

PROCEDURE AddSegment(t : T; READONLY segment : PicSegment.T) =
  BEGIN
    t.segments := PicSegmentList.Cons(segment, t.segments);
    t.extent   := PicExtent.Merge(t.extent, PicSegment.Extent(segment))
  END AddSegment;

PROCEDURE AddPoint(t : T; READONLY point : PicPoint.T) =
  BEGIN
    t.points := PicPointList.Cons(point, t.points);
    t.extent := PicExtent.Merge(t.extent, PicPoint.Extent(point))
  END AddPoint;

PROCEDURE AddCircle(t : T; READONLY circle : PicCircle.T) =
  BEGIN
    t.circles := PicCircleList.Cons(circle, t.circles);
    t.extent  := PicExtent.Merge(t.extent, PicCircle.Extent(circle))
  END AddCircle;

PROCEDURE AddText(t : T; READONLY text : PicText.T) =
  BEGIN
    t.texts  := PicTextList.Cons(text, t.texts);
    t.extent := PicExtent.Merge(t.extent, PicText.Extent(text))
  END AddText;

PROCEDURE Render(t : T; READONLY at : PicPoint.T; canvas : Canvas.T) =
  VAR
    mySize  := PicPoint.Minus(t.extent.ur, t.extent.ll);
    reqSize := t.curExtent();
    deltaO2 := PicPoint.Times(0.5d0, PicPoint.Minus(reqSize.ur, mySize));
    offset  := PicPoint.Plus(PicPoint.Plus(PicPoint.Times(-1.0d0, t.extent.ll),
                                           deltaO2),
                             at);
  BEGIN
    VAR
      p := t.points;
    BEGIN
      WHILE p # NIL DO
        IF doDebug THEN
          Debug.Out("PicSegments : point " & PicPoint.Format(p.head))
        END;
        canvas.point(PicPoint.Translate(p.head, offset));
        p := p.tail
      END
    END;
    VAR
      p := t.circles;
    BEGIN
      WHILE p # NIL DO
        IF doDebug THEN
          Debug.Out("PicSegments : circle " & PicCircle.Format(p.head))
        END;
        canvas.circle(PicCircle.Translate(p.head, offset));
        p := p.tail
      END
    END;
    VAR
      p := t.texts;
    BEGIN
      WHILE p # NIL DO
        IF doDebug THEN
          Debug.Out("PicSegments : text " & PicText.Format(p.head))
        END;
        canvas.text(PicText.Translate(p.head, offset));
        p := p.tail
      END
    END;
    VAR
      p := t.segments;
    BEGIN
      WHILE p # NIL DO
        IF doDebug THEN
          Debug.Out("PicSegments : segment " & PicSegment.Format(p.head))
        END;
        canvas.segment(PicSegment.Translate(p.head, offset));
        p := p.tail
      END
    END;
  END Render;

BEGIN END PicSegments.
