(* $Id: SXPrettyPrint.i3,v 1.1 2011/01/19 16:01:40 mika Exp $ *)

INTERFACE SXPrettyPrint;
IMPORT Wr, SX, Thread;

PROCEDURE Put(wr : Wr.T; sx : SX.T) RAISES { Wr.Failure, Thread.Alerted };

END SXPrettyPrint.
