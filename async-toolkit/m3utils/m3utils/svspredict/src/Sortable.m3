(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Sortable;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Compare(a, b : T) : CompRes = BEGIN RETURN a.compare(b) END Compare;

BEGIN END Sortable.
