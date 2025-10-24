(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicArray;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init() : T;
    put(x, y : CARDINAL; pic : Pic.T);
  END;

CONST Brand = "PicArray";

END PicArray.
  
