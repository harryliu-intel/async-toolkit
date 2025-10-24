(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: Matrix2LRType.m3,v 1.2 2008/02/04 00:10:46 mika Exp $ *)

MODULE Matrix2LRType;
IMPORT Fmt;
IMPORT Random;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.LongReal(t) END Format;

PROCEDURE Rand(r : Random.T) : T = 
  BEGIN RETURN r.longreal() END Rand;

BEGIN END Matrix2LRType.
