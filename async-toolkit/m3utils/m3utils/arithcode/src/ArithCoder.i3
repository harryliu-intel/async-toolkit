INTERFACE ArithCoder;
IMPORT ArithCallback;
IMPORT Rd;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    setCallback(cb : ArithCallback.T);
    char(c : CHAR);
    chars(READONLY a : ARRAY OF CHAR);
    text(t : TEXT);
    eof();
    rdTillEof(rd : Rd.T);
  END;

CONST Brand = "ArithCoder";

END ArithCoder.
