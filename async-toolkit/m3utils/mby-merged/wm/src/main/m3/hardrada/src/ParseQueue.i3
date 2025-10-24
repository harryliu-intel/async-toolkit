(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ParseQueue ;

IMPORT CARDINALList ;

TYPE T = CARDINALList.T ;

PROCEDURE Push( q : REF T ; el : CARDINAL := 0 ) ;

PROCEDURE Pop( q : REF T ) ;

PROCEDURE Inc( q : REF T ; inc : CARDINAL := 1 ) ;

PROCEDURE Peek( q : REF T ) : CARDINAL ;

PROCEDURE Depth( q : REF T ) : CARDINAL ;

PROCEDURE Sum( q : REF T ) : CARDINAL ;

PROCEDURE Debug( q : REF T ) ;

END ParseQueue .
