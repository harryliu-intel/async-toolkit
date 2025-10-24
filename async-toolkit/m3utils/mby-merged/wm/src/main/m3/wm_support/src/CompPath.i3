(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CompPath;
IMPORT CompRange;
IMPORT TextList;
IMPORT TextSet;

TYPE T = TextList.T; (* do not use -- hack *)

PROCEDURE ConfigureSet(s : TextSet.T);

VAR mu : MUTEX; (* use this if you use ConfigureSet *)

PROCEDURE Cat(a : T; b : TEXT) : T;

PROCEDURE CatArray(a : T; b : TEXT; i : CARDINAL) : T;

PROCEDURE Debug(reg : T; at : CompRange.T);

PROCEDURE Empty() : T;

PROCEDURE ToText(t : T) : TEXT;

PROCEDURE One(txt : TEXT) : T;
  
END CompPath.
