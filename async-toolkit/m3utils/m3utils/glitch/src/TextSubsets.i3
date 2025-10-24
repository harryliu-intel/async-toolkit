(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TextSubsets;
IMPORT TextSet;

TYPE Elem = TEXT;
     Set  = TextSet.T;

PROCEDURE Iterate(s : Set) : Iterator;

TYPE
  Iterator <: PubIterator;

  PubIterator = OBJECT METHODS
    next(VAR ss : Set) : BOOLEAN;
  END;

CONST Brand = "TextSubsets";

END TextSubsets.
