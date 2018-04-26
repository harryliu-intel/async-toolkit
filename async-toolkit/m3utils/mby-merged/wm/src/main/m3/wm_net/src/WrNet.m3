MODULE WrNet;
IMPORT Wr;
IMPORT Word;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32;

PROCEDURE PutU8(wr : Wr.T; u : U8)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    PutByte(wr, u, 0);
  END PutU8;

PROCEDURE PutU16(wr : Wr.T; u : U16)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    PutByte(wr, u, 8);
    PutByte(wr, u, 0);
  END PutU16;

PROCEDURE PutU32(wr : Wr.T; u : U32)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    PutByte(wr, u, 24);
    PutByte(wr, u, 16);
    PutByte(wr, u, 8);
    PutByte(wr, u, 0);
  END PutU32;

PROCEDURE PutByte(wr              : Wr.T;
                  READONLY w      : Word.T;
                  b               : [0..BITSIZE(Word.T)-8]) 
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutChar(wr, VAL(Word.Extract(w, b, 8),CHAR))
  END PutByte;

BEGIN END WrNet.
