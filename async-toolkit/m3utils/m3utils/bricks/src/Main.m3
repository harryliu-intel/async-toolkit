(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;

IMPORT Math;
IMPORT Debug;


CONST (* in meters *)
  BrickHeight = 63.0d-3;
  BrickLength = 230.0d-3;
  BrickWidth  = 114.0d-3;
  BrickKerf   = 2.0d-3; (* just a guess for now *)

  InternalRadius = 21.0d0 * 0.0254d0; (* 21 inches *)

  (* how to represent the bricks?  we want to be able to add faces, by
     cutting through a face *)


BEGIN

END Main.
