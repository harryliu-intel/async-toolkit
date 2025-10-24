(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE SpiceInstance;
IMPORT SpiceObject, Word;

TYPE
  T <: Public;

  Public = OBJECT
    flatName : TEXT;
    obj : SpiceObject.T; (* hierarchical instance *)
    parent : T;
  METHODS
    init(flatName : TEXT; obj : SpiceObject.T; parent : T) : T;
  END;

CONST Brand = "SpiceInstance";

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

END SpiceInstance.
