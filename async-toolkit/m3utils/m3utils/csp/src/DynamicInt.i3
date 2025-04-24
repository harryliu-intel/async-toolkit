INTERFACE DynamicInt;



IMPORT Word;
IMPORT Lex, FloatMode;

CONST Brand = "DynamicInt";

TYPE T        <: Public;
TYPE Natural  = T;

TYPE Public = ROOT;

TYPE CompRet = [-1..1];

EXCEPTION DivisionByZero;

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE New(x : INTEGER) : T;
PROCEDURE Copy(t : T) : T; (* this makes no sense, right? *)
PROCEDURE Div(a, b : T) : T RAISES { DivisionByZero };
PROCEDURE Mul(a, b : T) : T;
PROCEDURE Add(a, b : T) : T;
PROCEDURE Pow(b, x : T) : T RAISES { DivisionByZero };
PROCEDURE Sub(a, b : T) : T;
PROCEDURE Abs(a : T) : T;
PROCEDURE Mod(a, b : T) : T RAISES { DivisionByZero };
PROCEDURE Sign(a : T) : CompRet;
PROCEDURE Neg(a : T) : T;

(* bitwise ops *)
PROCEDURE GetBit(t : T; bit : CARDINAL) : [ 0 .. 1 ];
  (* returns the value of t{bit} assuming t is represented as 2's complement
     (which it isn't actually, internally) *)

PROCEDURE GetAbsMsb(t : T) : [-1..LAST(CARDINAL)];
  (* get the highest bit set in Abs(t) :
     returns -1 for Zero
  *)

PROCEDURE GetMsb(t : T) : [-1..LAST(CARDINAL)];
  (* get the highest bit in the 2s complement representation of t
     that is different from the sign bit *)

PROCEDURE Or(a, b : T) : T;
PROCEDURE And(a, b : T) : T;
PROCEDURE Xor(a, b : T) : T;
PROCEDURE Not(a : T) : T;
  

PROCEDURE Shift(a : T; sa : INTEGER) : T;
PROCEDURE RightShift(a : T; sa : CARDINAL) : T;
PROCEDURE LeftShift(a : T; sa : CARDINAL) : T;

CONST
  HexChars = ARRAY OF CHAR{'0','1','2','3','4','5','6','7','8','9',
                           'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                           'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                           'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                           'Y', 'Z' };

  MaxPrintBase = NUMBER(HexChars);

TYPE
  PrintBase = [ 2 .. MaxPrintBase ];
  
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

PROCEDURE Hash(a : T) : Word.T;

EXCEPTION OutOfRange;
  
PROCEDURE ToLongReal(a : T) : LONGREAL;
PROCEDURE ToInteger(a : T) : INTEGER RAISES { OutOfRange };
PROCEDURE Max(a, b : T) : T;
PROCEDURE Min(a, b : T) : T;

PROCEDURE Divide(dividend, divisor : T; VAR quotient, modulo : T)
  RAISES { DivisionByZero };
  (* returns quotient in q and modulo in m, using the regular rules for
     modulo (the modulo is always been the divisor and zero) *)

PROCEDURE Odd(a : T) : BOOLEAN;
PROCEDURE Even(a : T) : BOOLEAN;
  
PROCEDURE IsT(x : REFANY) : BOOLEAN;

PROCEDURE UniqReferences(to : BOOLEAN) : BOOLEAN;
  (* if called with TRUE, all calls returning the same value will 
     return the same reference, else that is not guaranteed *)


(* the following are used only for debugging *)
  
PROCEDURE GetRepBase() : T; 

PROCEDURE GetInitialized() : BOOLEAN;

PROCEDURE DebugT(t : T) : TEXT;

PROCEDURE Uniq(t : T) : T; (* internal use, but no harm in exporting it *)
       


REVEAL 
  T = Public BRANDED Brand OBJECT
    sign : [ -1 .. 1 ];
    rep  : NSeq;
  END;

CONST Chunk = 4; (* make it easy to debug in hex *)
      BaseLog2 =   ((BITSIZE(Word.T) DIV 2 - 1) DIV Chunk) * Chunk;
      (* (*was*) 10 *)
      
      Base     =  Word.Shift(1, BaseLog2); 
      (* must be less than or equal to sqrt(LAST(CARDINAL)) *)
      (* must be power of 2 *)

      WordMask   =  Base - 1;
      WordUnmask =  Word.Not(WordMask);


(* other sequence impl *)

TYPE 
  NArry = REF ARRAY OF Word.T;  (* hmm... *)

  NSeq = RECORD
    siz  : CARDINAL; (* # of significant digits *)
    a    : NArry;
  END;
PROCEDURE InitN(VAR s : NSeq; hintSize : CARDINAL) ;
PROCEDURE ShiftLeftInternalN(VAR s : NSeq; sa : CARDINAL) ;
PROCEDURE ShiftRightInternalN(VAR s : NSeq; sa : CARDINAL) ;
PROCEDURE ExtendN(VAR s : NSeq; toDigits : CARDINAL) ;
PROCEDURE CopyN(READONLY s : NSeq) : NSeq ;
PROCEDURE ClearTop(VAR s : NSeq) ; 

PROCEDURE StuffTheBits((*MODIFIES*)VAR wa : ARRAY OF Word.T) : T ;

PROCEDURE GetTheBits(a : T; VAR res : ARRAY OF Word.T) ;


END DynamicInt.
