INTERFACE ArithCode;
IMPORT ArithCoder;
IMPORT FreqTable;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(READONLY frequencies : FreqTable.T) : T;
    newEncoder() : ArithCoder.T;
    newDecoder() : ArithCoder.T;
  END;

CONST Brand = "ArithCode";

END ArithCode.
