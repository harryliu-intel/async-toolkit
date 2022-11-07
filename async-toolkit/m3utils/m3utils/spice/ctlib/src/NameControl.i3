INTERFACE NameControl;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT CardSeq;
IMPORT Pathname;
IMPORT Wr;
IMPORT TextCardTbl;

PROCEDURE MakeIdxMap(nameIdTbl     : TextCardTbl.T;
                     restrictNodes : TextSet.T;
                     regExList     : RegExList.T;
                     names         : TextSeq.T) : CardSeq.T;

  (* for given fsdbNames table, generate:

     names, restrictNodes, regExList
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

                     VAR wdWr      : REF ARRAY OF Wr.T;
                     VAR wdPth     : REF ARRAY OF Pathname.T;

                     includeIdNames := TRUE) : CARDINAL;

PROCEDURE FileIndex(nFiles, nNodes, nodeIndex : CARDINAL) : CARDINAL;
  (* get the file number of the node identified by nodeIndex.
     need to know how many nodes and how many files are present in 
     system *)
  
END NameControl.
  
