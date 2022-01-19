INTERFACE Aliases;
IMPORT Rd;
IMPORT Thread;

TYPE
  Tokenizer <: PubTokenizer;

  PubTokenizer = OBJECT METHODS
    init(rd : Rd.T) : Tokenizer;
    token(VAR tok : TEXT; VAR sep : CHAR) : BOOLEAN
      RAISES { Rd.Failure, Thread.Alerted };
    whatLine() : CARDINAL;
  END;

CONST Brand = "Aliases";

END Aliases.
