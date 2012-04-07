(* $Id$ *)

INTERFACE CSVParse;
IMPORT Rd, FloatMode, Lex, Thread;

EXCEPTION EndOfLine;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T;

    startLine() RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
    (* must be called at start, too *)

    cell() : TEXT RAISES { EndOfLine };
    
    int() : INTEGER RAISES { EndOfLine, FloatMode.Trap, Lex.Error };

    lr() : LONGREAL RAISES { EndOfLine, FloatMode.Trap, Lex.Error };

    whatLine() : CARDINAL;

    lastCell() : TEXT;
    
    lastLine() : TEXT;
  END;    

CONST Brand = "CSVParse";

END CSVParse.

    
