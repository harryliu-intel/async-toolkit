(* $Id: BDDScanner.i3,v 1.1 2014/02/09 11:16:08 mika Exp $ *)

INTERFACE BDDScanner;
IMPORT BDDBDDTbl;
IMPORT BDD;
IMPORT BDDCardTbl;

PROCEDURE Do(tbl : BDDBDDTbl.T) : T;

TYPE 
  T = RECORD
    pre : Stats;
    dec : Stats;
  END;

  Stats = RECORD
    nBdds                : CARDINAL;
    meanFanins           : LONGREAL;
    minFanins, maxFanins : CARDINAL;
    biggest              : BDD.T;
    biggestLit           : BDD.T;

    maxFanout            : CARDINAL;
    maxFanoutLit         : BDD.T;

    cnts                 : BDDCardTbl.T;
  END;

END BDDScanner.
