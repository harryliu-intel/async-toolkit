INTERFACE NameControl;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT CardSeq;
IMPORT Pathname;
IMPORT Wr;

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

PROCEDURE CountActiveNames(seq : CardSeq.T) : CARDINAL;

PROCEDURE WriteNames(wd, ofn       : Pathname.T;

                     names         : TextSeq.T;

                     idxMap        : CardSeq.T;
                     (* map of input node to output node *)
                     
                     maxFiles      : CARDINAL;

                     VAR nFiles    : CARDINAL;

                     VAR wdWr      : REF ARRAY OF Wr.T) : CARDINAL;
END NameControl.
  
