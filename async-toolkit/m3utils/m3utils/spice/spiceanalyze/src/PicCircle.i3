(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicCircle;
IMPORT PicPoint, PicCoord;
IMPORT PicExtent;

TYPE
  T = RECORD
    at : PicPoint.T;
    r  : PicCoord.NonNeg;
  END;

CONST Brand = "PicCircle";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Extent(READONLY a : T) : PicExtent.T;

PROCEDURE Translate(READONLY a : T; READONLY by : PicPoint.T) : T;

PROCEDURE Format(READONLY a : T) : TEXT;

END PicCircle.
