INTERFACE ServerPacket;
IMPORT ByteSeq;
IMPORT Wr, Thread, Rd;
IMPORT NetContext;
IMPORT Byte;
IMPORT Word;

(* it makes sense to build packets backwards, adding headers to the head
   of packet as we pop out.

   this is inconvenient while at the same time maintaining regular byte
   ordering, especially when maintaining the same representation for
   received packets, which are generally parsed head to tail.

   this auxiliary interface adds a special capability to a "ByteSeq.T":
   the ability efficiently to "make space" at the head of the sequence.

   The thus-prepared sequence can then be passed to a packet builder, which
   can simply use s.put() to fill in the needed bytes.  The expected 
   code sequence is:

   WITH headerSz = .. DO
     s.prepPfx(headerSz);
     FOR i := 0 TO headerSz-1 DO
       s.put(i, ...)
     END
   END
*)
TYPE
  T <: Public;

  Public = ByteSeq.T OBJECT METHODS
    prepPfx(pfxSz : CARDINAL); (* set up for a prefix to be added *)
  END;


TYPE End = { Front, Back };

PROCEDURE PutE(t : T; e : End; c : Byte.T);

PROCEDURE Put(t : T; i : CARDINAL; c : Byte.T);

PROCEDURE Get(t : T; i : CARDINAL) : Byte.T;

PROCEDURE Transmit(t : T; wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE FromRd(t : T; rd : Rd.T; VAR cx : NetContext.T) : T
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted };
  (* read from rd for as many bytes as allowed by cx and add to end of t *)
  (* returns itself *)

PROCEDURE ExtractBits(t                      : T;
                      byteOffset, startBit   : CARDINAL;
                      numBits                : [0..BITSIZE(Word.T)];
                      VAR w                  : Word.T) : BOOLEAN;
  (* starting from t[byteOffset],
     get bits from startBit and numBits consecutive.

     in case of overrun, return value is false

     little-endian extraction.
  *)

PROCEDURE ArrPut(VAR a     : ARRAY OF Byte.T;
                 startBit  : CARDINAL;
                 w         : Word.T;
                 numBits   : CARDINAL);

PROCEDURE PutA(t : T; e : End; READONLY a : ARRAY OF Byte.T);
  
CONST Brand = "ServerPacket";

END ServerPacket.
