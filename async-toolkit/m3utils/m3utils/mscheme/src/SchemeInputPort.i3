(* $Id$ *)

INTERFACE SchemeInputPort;
IMPORT Rd, Scheme;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T;

    readChar() : Scheme.Object;
    peekChar() : Scheme.Object;
    pushChar(ch : INTEGER);
    popChar() : INTEGER;
    peekCh() : INTEGER;
    read() : Scheme.Object;

    close() : Scheme.Boolean; (* Norvig has Scheme.Object *)
  END;

PROCEDURE IsEOF(x : Scheme.Object) : BOOLEAN;

CONST Brand = "SchemeInputPort";
    
END SchemeInputPort.
