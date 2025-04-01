(* $Id: BigInt.i3,v 1.5 2003/08/21 02:05:32 kp Exp $
   For complete documentation, see IntForRat.i3.
   There is practically no size limitation on "BigInt.T"s, however.
*)
INTERFACE BigInt;
IMPORT Word;
IMPORT Lex, FloatMode;

CONST Brand = "BigInt";

TYPE T        <: Public;
TYPE Natural  = T;

TYPE Public = ROOT;

TYPE CompRet = [-1..1];

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE New(x : INTEGER) : T;
PROCEDURE Copy(t : T) : T; (* this makes no sense, right? *)
PROCEDURE Div(a, b : T) : T;
PROCEDURE Mul(a, b : T) : T;
PROCEDURE Add(a, b : T) : T;
PROCEDURE Pow(b, x : T) : T;
PROCEDURE Sub(a, b : T) : T;
PROCEDURE Abs(a : T) : T;
PROCEDURE Mod(a, b : T) : T;
PROCEDURE Sign(a : T) : CompRet;
PROCEDURE Neg(a : T) : T;

CONST
  HexChars = ARRAY OF CHAR{'0','1','2','3','4','5','6','7','8','9',
                           'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                           'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                           'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                           'Y', 'Z' };

  MaxBase = NUMBER(HexChars);

TYPE
  PrintBase = [ 2 .. MaxBase ];
  
PROCEDURE Format(a : T; base : PrintBase := 10) : TEXT;

PROCEDURE Scan     (text : TEXT; base : PrintBase := 10; defNeg := FALSE) : T
  RAISES { Lex.Error };
  (* scan a text as a T, using the base as the default scanning base.
     defNeg flips the sign, if TRUE. *)

PROCEDURE ScanBased(text : TEXT; defaultBase : PrintBase := 10) : T
  RAISES { Lex.Error, FloatMode.Trap };
  
CONST DelimChars = SET OF CHAR { '_', ',', ' ', '\t' };

PROCEDURE ScanDelimited(text : TEXT; base : PrintBase := 10) : T
  RAISES { Lex.Error }; (* as Scan, but ignore DelimChars *)

VAR (* CONST *) Zero, One, Two : T;

PROCEDURE Hash(a : T) : Word.T;

EXCEPTION OutOfRange;
  
PROCEDURE ToLongReal(a : T) : LONGREAL;
PROCEDURE ToInteger(a : T) : INTEGER RAISES { OutOfRange };
PROCEDURE Max(a, b : T) : T;
PROCEDURE Min(a, b : T) : T;
PROCEDURE Divide(a, b : T; VAR q, r : T);

PROCEDURE GetRepBase() : T;
PROCEDURE GetBit(t : T; bit : CARDINAL) : [ 0 .. 1 ];

PROCEDURE IsT(x : REFANY) : BOOLEAN;

PROCEDURE UniqReferences(to : BOOLEAN) : BOOLEAN;
  (* if called with TRUE, all calls returning the same value will 
     return the same reference, else that is not guaranteed *)
  
END BigInt.
