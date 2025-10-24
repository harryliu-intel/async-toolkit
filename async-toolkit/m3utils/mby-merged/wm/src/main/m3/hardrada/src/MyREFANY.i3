(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE MyREFANY ;
CONST Brand = "REFANY" ;
TYPE T = REFANY ;
PROCEDURE Equal( ref1 , ref2 : T ) : BOOLEAN ;
END MyREFANY .
