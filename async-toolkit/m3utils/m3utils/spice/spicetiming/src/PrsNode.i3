(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE PrsNode;
IMPORT Word;

TYPE
  T <: ROOT;

CONST Brand = "PrsNode";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

END PrsNode.
      
  
