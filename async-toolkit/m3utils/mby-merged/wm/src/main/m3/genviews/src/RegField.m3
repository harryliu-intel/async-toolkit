(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE RegField;
IMPORT Integer;

PROCEDURE Compare(a, b : T) : [-1..1] =
  BEGIN RETURN Integer.Compare(a.lsb,b.lsb) END Compare;

BEGIN END RegField.
