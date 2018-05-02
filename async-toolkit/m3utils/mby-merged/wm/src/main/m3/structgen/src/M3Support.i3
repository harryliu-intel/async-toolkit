INTERFACE M3Support;
IMPORT Word;
IMPORT Lex, FloatMode;

PROCEDURE ParseUnsigned(txt : TEXT) : Word.T
  RAISES { Lex.Error, FloatMode.Trap } ;
  (* parse a number in Modula-3 style base_number *)

PROCEDURE Modula3Type(ofBits : [1..BITSIZE(Word.T)]) : TEXT;
  (* print out a Modula-3 type spec for a given width integer *)

CONST Brand = "M3Support";
      
END M3Support.
  
