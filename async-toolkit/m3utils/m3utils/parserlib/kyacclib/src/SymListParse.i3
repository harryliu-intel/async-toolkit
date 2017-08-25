(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: SymListParse.i3,v 1.2 2001/07/29 21:58:03 kp Exp $ *)

INTERFACE SymListParse;
IMPORT Rd, SymList;
IMPORT CharRange;

PROCEDURE BackGetName(rd: Rd.T): TEXT;
(* unget a char and get a word *)

PROCEDURE Parse(rd: Rd.T; allowedChars: CharRange.T): SymList.T;
(* parse list until end of line *)

END SymListParse.
