(* $Id$ *)

INTERFACE FileUtils;
IMPORT Pathname, OSError, Thread, Rd, Wr;

PROCEDURE Copy(from, to : Pathname.T) RAISES 
  { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure };

END FileUtils.
