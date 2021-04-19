(* $Id$ *)

INTERFACE DatabaseUtils;
IMPORT Rd, Thread, Wr, Database;

PROCEDURE Sanitize(string : TEXT) : TEXT;

TYPE ByteA = Database.ByteA;

PROCEDURE Text2ByteA(data : TEXT) : ByteA;
  (* escape special chars, see 
     http://www.postgresql.org/docs/7.4/static/datatype-binary.html
  *)
PROCEDURE Rd2ByteA(rd : Rd.T) : ByteA RAISES { Rd.Failure, Thread.Alerted };
  (* as above, but from a reader *)

PROCEDURE ByteA2Text(data : ByteA) : TEXT;
  (* reverse of above *)

PROCEDURE ByteA2Wr(wr : Wr.T; data : ByteA) RAISES { Wr.Failure };
  (* as above, but to a writer *)

PROCEDURE ByteA2TextRdWr(in : Rd.T; out : Wr.T) RAISES { Rd.Failure,
                                                         Thread.Alerted,
                                                         Wr.Failure };
  (* reading bytea from a reader, convert into text to a writer.
     leaves the writer un-closed at end! *)
PROCEDURE Text2ByteARdWr(in : Rd.T; out : Wr.T) RAISES { Rd.Failure,
                                                         Thread.Alerted,
                                                         Wr.Failure };
  (* reading text from a reader, convert into bytea to a writer.
     leaves the writer un-closed at end! *)

PROCEDURE FilterUnprintable(t : TEXT) : TEXT;
  (* for printing debugging *)
  
PROCEDURE Text2ByteAArray(READONLY in : ARRAY OF CHAR;
                          VAR out : ARRAY OF CHAR) : CARDINAL;
  (* returns # of bytes taken in output array, may take four times as
     many bytes in the output *)

END DatabaseUtils.
