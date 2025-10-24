(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PicPoint;
IMPORT Word;
IMPORT PicCoord;

TYPE
  T = RECORD x, y : PicCoord.T END;

  ExtentT = RECORD ll, ur : T END;

CONST
  Zero = T { PicCoord.Zero, PicCoord.Zero };

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Brand = "PicPoint";

PROCEDURE Extent(READONLY a : T) : ExtentT;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE Plus(READONLY a, b : T) : T;

PROCEDURE Times(a : LONGREAL; READONLY v : T) : T;
  
PROCEDURE Translate(READONLY a : T; READONLY by : T) : T;

PROCEDURE Format(READONLY a : T) : TEXT;
  
END PicPoint.
