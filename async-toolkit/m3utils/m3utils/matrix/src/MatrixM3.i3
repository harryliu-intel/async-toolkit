(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: MatrixM3.i3,v 1.1 2008/01/22 01:53:04 mika Exp $ *)

INTERFACE MatrixM3;
FROM Matrix IMPORT T, DimensionMismatch;

PROCEDURE MulD(a,b, prod : T)            RAISES { DimensionMismatch };

END MatrixM3.
