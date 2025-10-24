(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: FactorialIndex.i3,v 1.2 2005/04/05 09:09:52 mika Exp $ *)

INTERFACE FactorialIndex;
IMPORT Word;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(READONLY vi : ARRAY OF CARDINAL) : T;
    indices() : REF ARRAY OF CARDINAL;
    index(idx : CARDINAL) : CARDINAL;
  END;

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "FactorialIndex";

END FactorialIndex.
