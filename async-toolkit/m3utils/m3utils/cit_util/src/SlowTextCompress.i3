(* $Id: SlowTextCompress.i3,v 1.5 2010/07/08 08:35:58 mika Exp $ *)

INTERFACE SlowTextCompress;
IMPORT Rd, Wr, ProcUtils;

TYPE Mode = { Compress, Decompress };
CONST ModeNames = ARRAY Mode OF TEXT { "Compress", "Decompress" };

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT RAISES { ProcUtils.ErrorExit };

PROCEDURE RdWr(mode : Mode; in : Rd.T; out : Wr.T) RAISES { ProcUtils.ErrorExit };
  (* read from reader, write to write, and close both in and out *)

END SlowTextCompress.

