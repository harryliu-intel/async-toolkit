(* $Id$ *)

INTERFACE SchemeInputPort;
IMPORT Rd, SchemeObject, SchemeBoolean;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T;

    readChar() : SchemeObject.T;
    peekChar() : SchemeObject.T;
    pushChar(ch : INTEGER);
    popChar() : INTEGER;
    peekCh() : INTEGER;
    read() : SchemeObject.T;

    close() : SchemeBoolean.T; (* Norvig has Scheme.Object *)
  END;

PROCEDURE IsEOF(x : SchemeObject.T) : BOOLEAN;

CONST Brand = "SchemeInputPort";
    
END SchemeInputPort.
