INTERFACE URL;

IMPORT FloatMode, Lex;

PROCEDURE PlusToSpace(this : TEXT) : TEXT;
PROCEDURE Unescape(this : TEXT) : TEXT 
  RAISES { FloatMode.Trap, Lex.Error };

END URL.
