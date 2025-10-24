(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Pic;
IMPORT PicExtent;
IMPORT PicPoint;

REVEAL
  T = Public BRANDED Brand OBJECT
    reqExtent : PicExtent.T;
  OVERRIDES
    init := Init;
    setExtent := SetExtent;
    curExtent := CurExtent;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    RETURN t
  END Init;

PROCEDURE SetExtent(t : T; READONLY extent : PicExtent.T) =
  BEGIN
    t.reqExtent := PicExtent.T { PicPoint.Zero,
                                 PicPoint.Minus(extent.ur, extent.ll) }
  END SetExtent;

PROCEDURE CurExtent(t : T) : PicExtent.T =
  BEGIN RETURN t.reqExtent END CurExtent;
  
BEGIN END Pic.
