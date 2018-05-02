INTERFACE ServerPacket;
IMPORT CharSeq; (* should really by ByteSeq ! *)
IMPORT Wr, Thread, Rd;
IMPORT NetContext;

(* it makes sense to build packets backwards, adding headers to the head
   of packet as we pop out.

   this is inconvenient while at the same time maintaining regular byte
   ordering, especially when maintaining the same representation for
   received packets, which are generally parsed head to tail.

   this auxiliary interface adds a special capability to a "CharSeq.T":
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

  Public = CharSeq.T OBJECT METHODS
    prepPfx(pfxSz : CARDINAL); (* set up for a prefix to be added *)
  END;


TYPE End = { Front, Back };

PROCEDURE PutE(t : T; e : End; c : CHAR);

PROCEDURE Put(t : T; i : CARDINAL; c : CHAR);

PROCEDURE Get(t : T; i : CARDINAL) : CHAR;

PROCEDURE Transmit(t : T; wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE FromRd(t : T; rd : Rd.T; VAR cx : NetContext.T) : T
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted };
  (* read from rd for as many bytes as allowed by cx and add to end of t *)
  (* returns itself *)
  
CONST Brand = "ServerPacket";

END ServerPacket.
