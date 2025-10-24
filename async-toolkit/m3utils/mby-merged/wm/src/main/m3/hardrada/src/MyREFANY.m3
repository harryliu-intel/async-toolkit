(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE MyREFANY ;

PROCEDURE Equal( ref1 , ref2 : T ) : BOOLEAN =
BEGIN
	RETURN ref1 = ref2 ;
END Equal ;

BEGIN END MyREFANY .
