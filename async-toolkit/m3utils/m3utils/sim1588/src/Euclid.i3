(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Euclid;

PROCEDURE GCD(a, b : CARDINAL) : CARDINAL;

PROCEDURE Lowest(n, d : INTEGER; VAR nn, dd : INTEGER);

END Euclid.
