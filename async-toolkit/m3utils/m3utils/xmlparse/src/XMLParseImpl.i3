INTERFACE XMLParseImpl;
IMPORT Pathname;
IMPORT xmlParserContext;
FROM xmlParserTypes IMPORT StartProc, AttrProc, EndProc, CharDataProc;

TYPE
  PostProc     = PROCEDURE (c : xmlParserContext.T; s : REFANY);
  (* called after a chunk is done *)

PROCEDURE DoItImpl(p            : Pathname.T;
                   stuff        : REFANY;
                   start        : StartProc;
                   attr         : AttrProc;
                   end          : EndProc;
                   charData     : CharDataProc;
                   post         : PostProc (* may be NIL *));
  
END XMLParseImpl.
