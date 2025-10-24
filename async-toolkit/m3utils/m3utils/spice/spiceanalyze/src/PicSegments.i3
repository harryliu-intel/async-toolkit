(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicSegments;
IMPORT Pic;
IMPORT Refany;
IMPORT PicSegment;
IMPORT PicPoint;
IMPORT PicCircle;
IMPORT PicText;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init() : T;
    addSegment(READONLY seg  : PicSegment.T);
    addPoint  (READONLY at   : PicPoint.T);
    addCircle (READONLY circ : PicCircle.T);
    addText   (READONLY txt  : PicText.T);
  END;

CONST Brand = "PicSegments";

CONST Equal = Refany.Equal;

END PicSegments.
