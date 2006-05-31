(* $Id$ *)

INTERFACE SlowTextCompress;
IMPORT Rd, Wr;

TYPE Mode = { Compress, Decompress };

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT;

PROCEDURE RdWr(mode : Mode; in : Rd.T; out : Wr.T);
  (* read from reader, write to write, and close both in and out *)

END SlowTextCompress.
