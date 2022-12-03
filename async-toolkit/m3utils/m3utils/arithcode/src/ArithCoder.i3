INTERFACE ArithCoder;
IMPORT ArithCallback;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    setCallback(cb : ArithCallback.T);
    char(c : CHAR);
    chars(READONLY a : ARRAY OF CHAR);
    text(t : TEXT);
    eof();
  END;

CONST Brand = "ArithCoder";

END ArithCoder.
