MODULE RdNet;
IMPORT Rd;
IMPORT Word;
IMPORT Thread;
FROM NetTypes IMPORT U8, U16, U32;

PROCEDURE GetU8(rd : Rd.T) : U8 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U8 := 0;
  BEGIN
    res := GetByte(rd, res, 0);
    RETURN res
  END GetU8;

PROCEDURE GetU16(rd : Rd.T) : U16 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U16 := 0;
  BEGIN
    res := GetByte(rd, res, 8);
    res := GetByte(rd, res, 0);
    RETURN res
  END GetU16;

PROCEDURE GetU32(rd : Rd.T) : U32 
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    res : U32 := 0;
  BEGIN
    res := GetByte(rd, res, 24);
    res := GetByte(rd, res, 16);
    res := GetByte(rd, res, 8);
    res := GetByte(rd, res, 0);
    RETURN res
  END GetU32;

PROCEDURE GetByte(rd              : Rd.T;
                  READONLY w      : Word.T;
                  b               : [0..BITSIZE(Word.T)-8]) : Word.T
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    c : [0..16_ff];
  BEGIN
    c := ORD(Rd.GetChar(rd));
    RETURN Word.Insert(w, c, b, 8)
  END GetByte;

BEGIN END RdNet.
