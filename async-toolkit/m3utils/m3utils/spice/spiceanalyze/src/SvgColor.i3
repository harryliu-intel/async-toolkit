(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SvgColor;
IMPORT Word;

TYPE
  Channel = { R, G, B };

  Range   = [ 0 .. 255 ];

  T = ARRAY Channel OF Range;

CONST Brand = "SvgColor";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST
  Max   = LAST(Range);
  
  Black  = T { 0, .. };
  White  = T { Max, .. };
  
  Red    = T { Max, 0, 0 };
  Green  = T { 0, Max, 0 };
  Blue   = T { 0, 0, Max };

  Cyan    = T { 0, Max, Max };
  Magenta = T { Max, 0, Max };
  Yellow  = T { Max, Max, 0 };

PROCEDURE Format(READONLY a : T) : TEXT;
  
END SvgColor.
