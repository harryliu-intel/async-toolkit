(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicOverlay;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init(over, under : Pic.T) : T;
  END;

CONST Brand = "PicOverlay";

END PicOverlay.
