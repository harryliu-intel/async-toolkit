(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Pic;
IMPORT Refany;
IMPORT Canvas;
IMPORT PicPoint;
IMPORT PicExtent;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;

    minExtent() : PicExtent.T;
    (* extent of content, bottom up *)

    setExtent(READONLY to : PicExtent.T);
    (* extent to render, top down

       all that matters is { ur.x - ll.x, ur.y - ll.y }
    *)
    
    curExtent() : PicExtent.T;
    (* current extent to render *)
    
    render(READONLY at : PicPoint.T; canvas : Canvas.T);
    (* request to render, LL of cell to be rendered at at *)
  END;

CONST Brand = "Pic";

CONST Equal = Refany.Equal;
      
END Pic.
