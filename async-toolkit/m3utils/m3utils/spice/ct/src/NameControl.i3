INTERFACE NameControl;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT CardSeq;

PROCEDURE MakeIdxMap(names         : TextSeq.T;
                     restrictNodes : TextSet.T;
                     regExList     : RegExList.T) : CardSeq.T;

  (* for given names, restrictNodes, regExList
     generate: CardSeq that contains the mapping of each name to its output
     index.

     If no mapping, LAST(CARDINAL) will be placed.
  *)

CONST NoMapping = LAST(CARDINAL);

PROCEDURE SanitizeNames(idxMap : CardSeq.T;
                        names  : TextSeq.T);

END NameControl.
  
