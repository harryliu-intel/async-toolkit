INTERFACE SymListParse;
IMPORT Rd, SymList;
IMPORT CharRange;

PROCEDURE BackGetName(rd: Rd.T): TEXT;
(* unget a char and get a word *)

PROCEDURE Parse(rd: Rd.T; allowedChars: CharRange.T): SymList.T;
(* parse list until end of line *)

END SymListParse.
