(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicCanvas;

(* a canvas drawing into a PicSegments.T *)

IMPORT PicSegments;
IMPORT Canvas;

TYPE
  T <: Public;

  Public = Canvas.T OBJECT METHODS
    init(tgt : PicSegments.T) : T;
  END;

CONST Brand = "PicCanvas";

END PicCanvas.
