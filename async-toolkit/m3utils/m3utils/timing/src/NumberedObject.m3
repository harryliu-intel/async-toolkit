(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE NumberedObject;
IMPORT Integer;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN RETURN Integer.Compare(a.num, b.num) END Compare;

BEGIN END NumberedObject.
