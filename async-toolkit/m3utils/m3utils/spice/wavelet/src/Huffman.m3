(* XXX UNFINISHED XXX *)
MODULE Huffman;

REVEAL
  T = Public BRANDED Brand OBJECT
    rd : Rd.T;
    sz : CARDINAL;
  OVERRIDES
    init := Init;
    size := Size;
    read := Read;
  END;

PROCEDURE Write(wr : Wr.T; READONLY data : ARRAY OF CHAR)
  RAISES { Wr.Failure } =
  BEGIN
  END Write;

PROCEDURE Init(t : T; rd : Rd.T) : T RAISES { Rd.Failure } =
  BEGIN
  END Init;

PROCEDURE Size(t : T) : CARDINAL =
  BEGIN
    RETURN t.sz
  END Size;

PROCEDURE Read(t : T; VAR data : ARRAY OF CHAR)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
  END Read;

BEGIN END Huffman.

  

