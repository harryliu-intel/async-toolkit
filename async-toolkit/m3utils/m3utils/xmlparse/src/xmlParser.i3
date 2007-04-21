(* $Id$ *)

INTERFACE xmlParser;
IMPORT Ctypes;

TYPE
  StartCall = PROCEDURE (stuff : REFANY; el : Ctypes.const_char_star);
  AttrCall = PROCEDURE (stuff : REFANY; tag, attr : Ctypes.const_char_star);
  EndCall = PROCEDURE (stuff : REFANY);
  CharDataCall = PROCEDURE (stuff : REFANY; len : CARDINAL; data : Ctypes.const_char_star);

<*EXTERNAL*>
PROCEDURE xmlParserMain(path : Ctypes.const_char_star;
                        stuff : REFANY;
                        s : StartCall; a : AttrCall; e : EndCall;
                        c : CharDataCall) : INTEGER;
  (* returns 0 if OK, -1 if error *)

END xmlParser.
