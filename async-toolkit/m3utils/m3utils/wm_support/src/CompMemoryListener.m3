(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CompMemoryListener;
IMPORT Word;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hash() END Hash;
  
PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

BEGIN END CompMemoryListener.
