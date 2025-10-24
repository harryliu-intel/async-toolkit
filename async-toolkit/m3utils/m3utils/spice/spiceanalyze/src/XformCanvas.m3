(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE XformCanvas;
IMPORT Canvas, CanvasXform;
IMPORT PicPoint, PicCircle, PicText, PicSegment;
IMPORT PicCoord;

REVEAL
  T = Public BRANDED Brand OBJECT
    target : Canvas.T;
  OVERRIDES
    init := Init;
    
    point   := Point;
    circle  := Circle;
    text    := Text;
    segment := Segment;
  END;

TYPE
  D = Default;

PROCEDURE Init(t : T; target : Canvas.T) : T =
  BEGIN
    t.target := target;
    RETURN t
  END Init;

PROCEDURE Point(t : T; READONLY point : PicPoint.T) =
  VAR
    xpoint := point;
  BEGIN
    xpoint := t.xformPoint(point);
    t.target.point(xpoint)
  END Point;

PROCEDURE Circle(t : T; READONLY circle : PicCircle.T) =
  VAR
    xcirc := circle;
  BEGIN
    xcirc.at := t.xformPoint(circle.at);
    xcirc.r  := t.xformLength(circle.r);
    t.target.circle(xcirc)
  END Circle;

PROCEDURE Segment(t : T; READONLY segment : PicSegment.T) =
  VAR
    xseg := segment;
  BEGIN
    xseg.a := t.xformPoint(segment.a);
    xseg.b := t.xformPoint(segment.b);
    t.target.segment(xseg)
  END Segment;

PROCEDURE Text(t : T; READONLY text : PicText.T) =
  VAR
    xtext := text;
  BEGIN
    xtext.ll    := t.xformPoint(text.ll);
    xtext.width := t.xformLength(text.width);
    t.target.text(xtext)
  END Text;

  (**********************************************************************)

REVEAL
  Default = PublicDefault BRANDED Brand & " Default" OBJECT
    xform : CanvasXform.T;
  OVERRIDES
    init        := InitD;
    xformPoint  := XformPointD;
    xformLength := XformLengthD;
  END;


PROCEDURE InitD(t : D; target : Canvas.T; xform : CanvasXform.T) : T =
  BEGIN
    t.xform := xform;
    RETURN T.init(t, target)
  END InitD;

PROCEDURE XformPointD(t : D; p : PicPoint.T) : PicPoint.T =
  VAR
    xpoint := p;
  BEGIN
    xpoint.x := xpoint.x * t.xform.scale + t.xform.translateOrigin.x;
    xpoint.y := xpoint.y * t.xform.scale + t.xform.translateOrigin.y;
    RETURN xpoint
  END XformPointD;

PROCEDURE XformLengthD(t : D; l : PicCoord.T) : PicCoord.T =
  BEGIN
    RETURN l * t.xform.scale
  END XformLengthD;
  
BEGIN END XformCanvas.
