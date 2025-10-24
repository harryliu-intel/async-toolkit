(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RAC;
IMPORT Word;

TYPE T = REF ARRAY OF CHAR;
     B =     ARRAY OF CHAR;

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Compare(a, b : T) : [-1 .. 1];

PROCEDURE ToText(a : T) : TEXT;

PROCEDURE FromText(txt : TEXT) : T;

PROCEDURE FromB(READONLY b : B) : T;

PROCEDURE Sub(t : T; from : CARDINAL; len : CARDINAL := LAST(CARDINAL)) : T;

PROCEDURE SubIs(t, u : T; from : CARDINAL; len : CARDINAL := LAST(CARDINAL)) : BOOLEAN;

CONST Brand = "RAC / REF ARRAY OF CHAR";

END RAC.
