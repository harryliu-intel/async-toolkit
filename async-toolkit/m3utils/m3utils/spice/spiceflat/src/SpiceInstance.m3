(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpiceInstance;
IMPORT Word;
IMPORT Text;
IMPORT SpiceObject;
IMPORT Debug;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T; flatName : TEXT; obj : SpiceObject.T; parent : T) : T =
  BEGIN
    t.flatName := flatName;
    t.obj := obj;
    t.parent := parent;
    RETURN t
  END Init;
      
PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    IF TE(a.flatName, b.flatName) AND a.obj # b.obj THEN
      Debug.Error("SpiceInstance.Equal : multiple definitions of object named " & a.flatName)
    END;
    RETURN TE(a.flatName, b.flatName)
  END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Text.Hash(a.flatName) END Hash;

BEGIN END SpiceInstance.
