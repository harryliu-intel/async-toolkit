(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SourceTextSTRING;
IMPORT Text;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN RETURN Text.Equal(a, b) END Equal;

BEGIN END SourceTextSTRING.
