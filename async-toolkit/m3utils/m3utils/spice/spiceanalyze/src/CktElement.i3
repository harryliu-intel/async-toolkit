(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CktElement;
IMPORT Refany;
IMPORT Word;

TYPE T <: ROOT;
     
CONST Brand = "CktElement";

CONST Equal = Refany.Equal;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE Format(a : T) : TEXT;
  
END CktElement.
