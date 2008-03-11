(* $Id$ *)

INTERFACE TCPMaker;
IMPORT TCP, IP;

(* TCPMaker.Default : create new TCP.Ts out of a given target addres
           .Simple  : just return given TCP.T (a single one) *)

TYPE
  T = OBJECT METHODS
    makeTCP() : TCP.T RAISES { IP.Error };
  END;

  Default <: PubDefault;

  PubDefault = T OBJECT METHODS
    init(nameString : TEXT; defaultPort : IP.Port): Default RAISES { ConnErr };
  END;

  Simple <: PubSimple;

  PubSimple = T OBJECT METHODS
    init(tcp : TCP.T) : Simple;
  END;

EXCEPTION ConnErr(TEXT);

CONST Brand = "TCPMaker";

END TCPMaker.
