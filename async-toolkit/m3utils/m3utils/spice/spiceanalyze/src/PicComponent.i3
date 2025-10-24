(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicComponent;
IMPORT PicOverlay, CktElement;

TYPE
  T <: Public;

  Step = [ -1 .. +1 ];
  
  Public = PicOverlay.T OBJECT METHODS
    init(obj : CktElement.T) : T;
    setNeighbor(dx, dy : Step; n : T);
  END;

CONST Brand = "PicComponent";

END PicComponent.
