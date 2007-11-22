(* $Id$ *)

INTERFACE TCPMaker;
IMPORT TCP, IP;

TYPE
  T = OBJECT METHODS
    makeTCP() : TCP.T RAISES { IP.Error };
  END;

  Default <: PubDefault;

  PubDefault = T OBJECT METHODS
    init(nameString : TEXT; defaultPort : IP.Port): Default RAISES { ConnErr };
  END;

EXCEPTION ConnErr(TEXT);

CONST Brand = "TCPMaker";

END TCPMaker.
