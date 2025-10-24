(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TypeUse ;

CONST Brand = "TypeUse" ;

TYPE T = RECORD
	TypeName : TEXT := "" ;
	(* Blank implies no pointer points to this *)
	Ptr : REF ARRAY OF TEXT := NIL ; 
	AlreadyAssigned : BOOLEAN := FALSE ;
END ;

END TypeUse .
