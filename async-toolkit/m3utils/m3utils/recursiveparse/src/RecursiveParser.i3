INTERFACE RecursiveParser;
FROM ParseError IMPORT E;
FROM RecursiveLexer IMPORT Buffer, String;
IMPORT Text;

TYPE
  T <: Public;

  Public = OBJECT END;

CONST Brand = "RecursiveParser";

PROCEDURE GetToken(t : T; READONLY tok : ARRAY OF CHAR) : BOOLEAN ;
PROCEDURE Next(t : T) ;
PROCEDURE MustBeToken(t : T; READONLY tok : ARRAY OF CHAR) RAISES { E } ;
PROCEDURE MustNotBeChar(t : T; c : CHAR) RAISES { E } ;
PROCEDURE MustBeChars(t : T; READONLY a : ARRAY OF CHAR) RAISES { E } ;
PROCEDURE MustBeCharSet(t : T; s : SET OF CHAR; VAR c : CHAR) RAISES { E } ;
PROCEDURE BrackOrEmpty(txt : TEXT) : TEXT ;

PROCEDURE GetChar(t : T; c : CHAR) : BOOLEAN ;
PROCEDURE MustBeChar(t : T;  c : CHAR) RAISES { E } ;
PROCEDURE GetCharSet(t : T; s : SET OF CHAR; VAR c : CHAR) : BOOLEAN ;
PROCEDURE MustBeSingle(t : T; VAR c : CHAR) RAISES { E } ;

PROCEDURE GetSingle(t : T; VAR c : CHAR) : BOOLEAN ;

CONST A2T = Text.FromChars;
PROCEDURE S2T(READONLY buff : Buffer; s : String) : TEXT;

END RecursiveParser.
