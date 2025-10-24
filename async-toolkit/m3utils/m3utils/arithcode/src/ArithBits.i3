INTERFACE ArithBits;
IMPORT Word AS Super;

(* See Nelson, Mark.  Data compression with arithmetic coding.  2014. *)

TYPE T = Super.T;

CONST Bits         = BITSIZE(T);
      CodeBits     = Bits DIV 2 + 1;
      FreqBits     = Bits DIV 2 - 2;

      MaxCode      = Super.Shift(1, CodeBits) - 1;
      MaxFreq      = Super.Shift(1, FreqBits) - 1;

      OneHalf      = Super.Shift(1, CodeBits - 1);
      OneFourth    = Super.Shift(1, CodeBits - 2);
      ThreeFourths = Super.Plus(OneHalf, OneFourth);
      
TYPE
  Code = [ 0 .. MaxCode ];
  Freq = [ 0 .. MaxFreq ];

CONST Brand = "ArithBits";

      Plus   = Super.Plus;
      Times  = Super.Times;
      Minus  = Super.Minus;
      Divide = Super.Divide;
      LT     = Super.LT;
      GE     = Super.GE;
      Shift  = Super.Shift;
      And    = Super.And;
      
END ArithBits.
