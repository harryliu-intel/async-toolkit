(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MELRType;
IMPORT Math;

TYPE T = LONGREAL;

CONST Brand = "MELRType";

CONST Null = FIRST(LONGREAL);

PROCEDURE Format(READONLY a : T) : TEXT;

PROCEDURE Plus(READONLY a, b : T) : T;

PROCEDURE Minus(READONLY a, b : T) : T;

PROCEDURE ScalarMul(READONLY a, b : T) : T;

CONST Times = ScalarMul;

PROCEDURE Abs(READONLY a : T) : T;

CONST Sqrt = Math.sqrt;
      
PROCEDURE ZeroLT(lim : LONGREAL; READONLY a : T) : T;
  
END MELRType.
  
