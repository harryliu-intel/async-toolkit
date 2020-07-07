INTERFACE DefLexer;
FROM RecursiveParserRep IMPORT Buffer, State, String;

CONST BaseSpecial    = SET OF CHAR { '(', ')', '{', '}', '-', '+', ';' };
CONST DefDivChar = '/';
CONST DefBusbitChars = ARRAY [0..1] OF CHAR { '[', ']' };

CONST DefSpecial = BaseSpecial + 
                   SET OF CHAR { DefDivChar } + 
                   SET OF CHAR { DefBusbitChars[0] } + 
                   SET OF CHAR { DefBusbitChars[1] };

CONST Digit = SET OF CHAR { '0' .. '9' };

TYPE 
  T = RECORD
    special     := DefSpecial;
    divChar     := DefDivChar;
    busbitChars := DefBusbitChars;
  END;

PROCEDURE GetToken(READONLY t : T;
                   VAR buff  : Buffer;
                   VAR state : State;
                   VAR res   : String) : BOOLEAN;

PROCEDURE DividerChar(VAR s : T; c : CHAR);

PROCEDURE BusbitChars(VAR s : T; c : ARRAY [0..1] OF CHAR);

END DefLexer.
