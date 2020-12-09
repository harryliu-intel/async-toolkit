GENERIC INTERFACE Tokenizer(Defs);

(* defs must contain the following symbols:

BufSiz      : CARDINAL    -- CARDINAL size of input buffer
Special     : SET OF CHAR -- special characters to recognize as tokens
White       : SET OF CHAR -- whitespace characters to skip
Ident1      : SET OF CHAR -- legal initial chars for identifiers
Ident2      : SET OF CHAR -- legal non-initial chars for identifiers
CComments   : BOOLEAN     -- recognize (and skip) C-style comments
DoString    : BOOLEAN     -- recognize string
StringQuote : CHAR        -- quotation mark to use (backslash escapes)
*)

IMPORT Rd, Thread;

TYPE
  State = RECORD
    rd       : Rd.T;                       (* input stream *)

    progress := TRUE;                      (* config *)

    (* debugging variables *)
    lev      : CARDINAL := 0;
    lineno   : CARDINAL := 1;
    bytes    : CARDINAL := 0;

    b        : CARDINAL := 0;              (* buffer pointer  *)
    e        : CARDINAL := 0;              (* end of buffer   *)
    s        : CARDINAL := Defs.BufSiz;    (* start of token  *)

    haveTok  := FALSE;
    string   := FALSE;
  END;

  Buffer = ARRAY [ 0 .. Defs.BufSiz-1 ] OF CHAR;

EXCEPTION Syntax(CARDINAL); (* arg is line number of syntax error *)

TYPE Token = RECORD s, n : CARDINAL END;

PROCEDURE GetAny(VAR buf : ARRAY OF CHAR; VAR st : State;
                 VAR tok : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetExact(VAR buf : ARRAY OF CHAR; VAR st : State;
                   READONLY str : ARRAY OF CHAR) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetIdent(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR str : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetInt(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR int : INTEGER) : BOOLEAN 
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetFloat(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR lr : LONGREAL) : BOOLEAN 
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

CONST Brand = "Tokenizer(" & Defs.Brand & ")";
        
END Tokenizer.
