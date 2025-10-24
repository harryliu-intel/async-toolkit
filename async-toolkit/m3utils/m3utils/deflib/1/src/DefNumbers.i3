(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefNumbers;
IMPORT RecursiveParser;
FROM ParseError IMPORT E;

PROCEDURE MustBeInt(t : RecursiveParser.T; VAR i : INTEGER) RAISES { E };
PROCEDURE MustGetInt(t : RecursiveParser.T) : INTEGER RAISES { E };
PROCEDURE MustBeCard(t : RecursiveParser.T; VAR c : CARDINAL) RAISES { E }; 
PROCEDURE MustGetCard(t : RecursiveParser.T) : CARDINAL RAISES { E }; 
PROCEDURE GetCard(t : RecursiveParser.T; VAR c : CARDINAL) : BOOLEAN RAISES { E } ;

END DefNumbers.
