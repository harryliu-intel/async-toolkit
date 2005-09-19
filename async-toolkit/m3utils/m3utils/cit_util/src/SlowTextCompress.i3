(* $Id$ *)

INTERFACE SlowTextCompress;
IMPORT Rd, Wr;

TYPE Mode = { Compress, Decompress };

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT;

END SlowTextCompress.
