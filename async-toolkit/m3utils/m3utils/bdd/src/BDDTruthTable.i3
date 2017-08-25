(* $Id: BDDTruthTable.i3,v 1.1 2014/01/23 08:08:43 mika Exp $ *)

INTERFACE BDDTruthTable;
IMPORT CardSet, BDD;

PROCEDURE Make(x             : BDD.T;
               READONLY vars : ARRAY OF BDD.T;
               cols          : CardSet.T   
              ) : T;

TYPE
  T = RECORD
    rowVars : REF ARRAY OF BDD.T;
    colVars : REF ARRAY OF BDD.T;
    tt      : REF ARRAY OF ARRAY OF BOOLEAN;
  END;

END BDDTruthTable.
