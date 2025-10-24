(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE HTMLText;

REVEAL
  T = Public BRANDED "HTML Text" OBJECT
    text : TEXT;
  OVERRIDES
    init   := Init;
    format := Format;
  END;

PROCEDURE Init(self : T; text : TEXT) : T =
  BEGIN self.text := text; RETURN self END Init;

PROCEDURE Format(self : T) : TEXT = BEGIN RETURN self.text END Format;

BEGIN END HTMLText.
