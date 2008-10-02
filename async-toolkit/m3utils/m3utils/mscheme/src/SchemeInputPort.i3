(* $Id$ *)

INTERFACE SchemeInputPort;
IMPORT Rd, SchemeObject, SchemeBoolean, SchemeSymbol;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T;

    readChar() : SchemeObject.T;
    peekChar() : SchemeObject.T;
    pushChar(ch : INTEGER) : INTEGER;
    popChar() : INTEGER;
    peekCh() : INTEGER;
    read() : SchemeObject.T;

    close() : SchemeBoolean.T; (* Norvig has Scheme.Object *)
  END;

PROCEDURE IsEOF(x : SchemeObject.T) : BOOLEAN;

CONST Brand = "SchemeInputPort";

VAR (* CONST *) EOF : SchemeSymbol.T;
    
END SchemeInputPort.
