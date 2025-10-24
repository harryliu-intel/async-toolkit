(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id: LU2_F_sp.i3,v 1.1 2008/01/22 08:23:29 mika Exp $ *)

INTERFACE LU2_F_sp;
IMPORT RMatrix2 AS M;

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V);

END LU2_F_sp.
