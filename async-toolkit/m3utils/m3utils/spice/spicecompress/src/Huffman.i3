INTERFACE Huffman;
IMPORT Wr, Rd;

PROCEDURE Write(wr : Wr.T; READONLY data : ARRAY OF CHAR)
  RAISES { Wr.Failure };

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(rd : Rd.T) : T RAISES { Rd.Failure };
    size() : CARDINAL;
    read(VAR data : ARRAY OF CHAR) RAISES { Rd.Failure, Rd.EndOfFile };
  END;

CONST Brand = "Huffman";

END Huffman.
  
  
