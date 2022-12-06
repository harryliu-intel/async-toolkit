MODULE ArithCallback;
IMPORT Wr;

REVEAL
  Writer = PubWriter BRANDED Brand & " Writer" OBJECT
    wr : Wr.T;
  OVERRIDES
    init    := Init;
    newByte := NewByte;
    newEof  := NewEof;
  END;
  
PROCEDURE Init(writer : Writer; wr : Wr.T) : Writer =
  BEGIN
    writer.wr := wr;
    RETURN writer
  END Init;
  
PROCEDURE NewByte(writer : Writer; byte : CHAR) =
  BEGIN
    Wr.PutChar(writer.wr, byte)
  END NewByte;
  
PROCEDURE NewEof(writer : Writer) =
  BEGIN
    writer.wr := NIL
  END NewEof;

BEGIN END ArithCallback.
