INTERFACE FlatUI;
IMPORT TextSpiceInstanceSetTbl, TextTextSetTbl, TextTextTbl;

PROCEDURE REPL(assocs : TextSpiceInstanceSetTbl.T;
               symTab : TextTextSetTbl.T;
               canonTbl : TextTextTbl.T);

END FlatUI.
