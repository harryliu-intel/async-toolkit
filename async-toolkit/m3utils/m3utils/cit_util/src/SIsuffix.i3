(* $Id$ *)

INTERFACE SIsuffix;
IMPORT FloatMode, Lex;

EXCEPTION 
  OutOfRange;
  UnknownSuffix;

(* parse numbers that have appended SI suffixes. (or dont) *)

(* If the routines are called with a mode of Base10, the standard SI *)
(* suffixes will be used.  If Base2 is used, then k is 1024, etc.    *)

TYPE Mode = { Base10, Base2 };

PROCEDURE Int(text : TEXT; mode := Mode.Base10) : INTEGER RAISES { OutOfRange, UnknownSuffix, FloatMode.Trap, Lex.Error } ;

PROCEDURE LongReal(text : TEXT; mode := Mode.Base10) : LONGREAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error };

PROCEDURE Real(text : TEXT; mode := Mode.Base10) : REAL RAISES { UnknownSuffix, FloatMode.Trap, Lex.Error };

TYPE
  T = Suffix;

  Suffix = RECORD
    char : CHAR;
    size : LONGREAL;
    name : TEXT;
    geeky : LONGREAL := 0.0d0
  END;

(* n.b. deka is non-standard.  should be "da" according to SI *)
(* also micro should be "mu" (Greek letter) *)
(* kilo could perhaps be "K" as well *)

CONST
  KiloGeek = FLOAT(1024, LONGREAL);

  List = ARRAY OF T { 
    T { 'a' , 1.0d-18, "atto" } ,
    T { 'f' , 1.0d-15, "femto" } ,
    T { 'p' , 1.0d-12, "pico" } ,
    T { 'n' , 1.0d-9 , "nano" } ,
    T { 'u' , 1.0d-6 , "micro" } ,
    T { 'm' , 1.0d-3 , "milli" } ,
    T { 'c' , 1.0d-2 , "centi" } ,
    T { 'd' , 1.0d-1 , "deci" } ,
    T { 'D' , 1.0d1  , "deka" } ,
    T { 'h' , 1.0d2  , "hecto" } ,
    T { 'k' , 1.0d3  , "kilo", KiloGeek } ,
    T { 'M' , 1.0d6  , "mega", KiloGeek * KiloGeek } ,
    T { 'G' , 1.0d9  , "giga", KiloGeek * KiloGeek * KiloGeek } ,
    T { 'T' , 1.0d12 , "tera", KiloGeek * KiloGeek * KiloGeek * KiloGeek } ,
    T { 'P' , 1.0d15 , "peta", 
        KiloGeek * KiloGeek * KiloGeek * KiloGeek * KiloGeek } ,
    T { 'E' , 1.0d18 , "exa", 
        KiloGeek * KiloGeek * KiloGeek * KiloGeek * KiloGeek * KiloGeek } 
  };

  Brand = "SIsuffix";

END SIsuffix.

