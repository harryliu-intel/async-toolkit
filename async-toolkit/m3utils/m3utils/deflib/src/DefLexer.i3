INTERFACE DefLexer;
IMPORT Rd;

CONST BufSize = 4096;

CONST BaseSpecial    = SET OF CHAR { '(', ')', '{', '}', '-', '+', ';' };
CONST DefDivChar = '/';
CONST DefBusbitChars = ARRAY [0..1] OF CHAR { '[', ']' };

CONST DefSpecial = BaseSpecial + 
                   SET OF CHAR { DefDivChar } + 
                   SET OF CHAR { DefBusbitChars[0] } + 
                   SET OF CHAR { DefBusbitChars[1] };

CONST Digit = SET OF CHAR { '0' .. '9' };

TYPE 
  State = RECORD
    rd : Rd.T;
    s    := 0;     (* next token to parse starts here *)
    e    := 0;     (* end of currently valid chars in buffer *)
    eof  := FALSE; (* done parsing *)
    line := 1;
  END;

  T = RECORD
    special     := DefSpecial;
    divChar     := DefDivChar;
    busbitChars := DefBusbitChars;
  END;

TYPE Buffer = ARRAY [0..BufSize-1] OF CHAR;

TYPE String = RECORD start : [0..BufSize-1]; n : [0..BufSize] END;

PROCEDURE GetToken(READONLY t : T;
                   VAR buff  : Buffer;
                   VAR state : State;
                   VAR res   : String) : BOOLEAN;

PROCEDURE DividerChar(VAR s : T; c : CHAR);

PROCEDURE BusbitChars(VAR s : T; c : ARRAY [0..1] OF CHAR);

END DefLexer.
