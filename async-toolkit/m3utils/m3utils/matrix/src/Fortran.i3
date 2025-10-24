(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: Fortran.i3,v 1.1 2005/04/20 00:08:49 mika Exp $ *)

INTERFACE Fortran;

PROCEDURE Sign(a, b : LONGREAL) : LONGREAL;

PROCEDURE pythag(a, b : LONGREAL) : LONGREAL;

END Fortran.
