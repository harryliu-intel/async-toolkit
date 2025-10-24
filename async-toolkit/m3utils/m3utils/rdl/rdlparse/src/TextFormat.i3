(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TextFormat;
IMPORT Text;

TYPE T = Text.T;
     
CONST Equal   = Text.Equal;
      Hash    = Text.Hash;
      Compare = Text.Compare;

CONST Brand = "TextFormat";

PROCEDURE Format(t : TEXT) : TEXT; (* identity *)

END TextFormat.
     
