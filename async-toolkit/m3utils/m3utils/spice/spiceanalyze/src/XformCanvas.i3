(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE XformCanvas;
IMPORT Canvas, CanvasXform;
IMPORT PicPoint, PicCoord;

TYPE

  T <: Public;

  Public = Canvas.T OBJECT METHODS
    init(target : Canvas.T) : T;

    (* override the following *)
    xformPoint(p : PicPoint.T) : PicPoint.T; 
    xformLength(p : PicCoord.T) : PicCoord.T;
  END;

  Default <: PublicDefault;

  PublicDefault = T OBJECT METHODS
    init(target : Canvas.T; xform : CanvasXform.T) : T;
  END;
  
CONST Brand = "XformCanvas";

END XformCanvas.
