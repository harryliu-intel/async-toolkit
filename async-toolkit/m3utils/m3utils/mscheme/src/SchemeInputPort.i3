(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeInputPort;
IMPORT Rd, SchemeObject, SchemeBoolean, SchemeSymbol;
FROM Scheme IMPORT E;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T;

    readChar() : SchemeObject.T;
    peekChar() : SchemeObject.T;
    pushChar(ch : INTEGER) : INTEGER;
    popChar() : INTEGER;
    peekCh() : INTEGER;
    read() : SchemeObject.T RAISES { E };

    close() : SchemeBoolean.T RAISES { E }; (* Norvig has Scheme.Object *)
  END;

PROCEDURE IsEOF(x : SchemeObject.T) : BOOLEAN;

CONST Brand = "SchemeInputPort";

VAR (* CONST *) EOF : SchemeSymbol.T;
    
END SchemeInputPort.
