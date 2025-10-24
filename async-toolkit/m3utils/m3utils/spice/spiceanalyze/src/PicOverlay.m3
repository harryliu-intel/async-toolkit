(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE PicOverlay;
IMPORT Pic;
IMPORT PicExtent;
IMPORT Canvas;
IMPORT PicPoint;

REVEAL
  T = Public BRANDED Brand OBJECT
    over, under : Pic.T;
  OVERRIDES
    init      := Init;
    
    minExtent := MinExtent;
    setExtent := SetExtent;
    curExtent := CurExtent;
    render    := Render;
  END;

PROCEDURE Init(t : T; over, under : Pic.T) : T =
  BEGIN
    t.over := over;
    t.under := under;
    RETURN t
  END Init;

PROCEDURE MinExtent(t : T) : PicExtent.T =
  BEGIN
    RETURN PicExtent.Merge(t.over.minExtent(),
                           t.under.minExtent())
  END MinExtent;
  
PROCEDURE SetExtent(t : T; READONLY to : PicExtent.T) =
  BEGIN
    t.over.setExtent(to);
    t.under.setExtent(to)
  END SetExtent;

PROCEDURE CurExtent(t : T) : PicExtent.T =
  BEGIN
    RETURN t.over.curExtent();
  END CurExtent;

PROCEDURE Render(t : T; READONLY at : PicPoint.T; canvas : Canvas.T) =
  BEGIN
    t.under.render(at, canvas);
    t.over.render(at, canvas)
  END Render;

BEGIN END PicOverlay.
