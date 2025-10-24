(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RegContainer;
IMPORT RegRegfile;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    skipArc := SkipArc;
  END;

PROCEDURE SkipArc(t : T) : BOOLEAN =
  BEGIN RETURN ISTYPE(t, RegRegfile.T) AND t.children.size() = 1 END SkipArc;

BEGIN END RegContainer.
