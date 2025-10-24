(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Dims;
IMPORT Word;

TYPE
  T = ARRAY OF CARDINAL;

CONST
  Scalar = T { };

CONST Brand = "Dims";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Clone(READONLY a : T) : REF T; (* all zero *)

TYPE 
  Iterator <: PubIterator;
  
  PubIterator = OBJECT METHODS
    init(READONLY lim : T; downward := FALSE) : Iterator;
    next(VAR nxt : T) : BOOLEAN;
  END;

PROCEDURE Iterate(READONLY t : T; downward := FALSE) : Iterator;

PROCEDURE Format(READONLY z : T) : TEXT;

END Dims.

