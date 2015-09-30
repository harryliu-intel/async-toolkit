(* $Id: FileUtils.i3,v 1.2 2007/05/01 07:47:41 mika Exp $ *)

INTERFACE FileUtils;
IMPORT Pathname, OSError, Thread, Rd, Wr;

PROCEDURE Copy(from, to : Pathname.T) RAISES 
  { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure };

PROCEDURE Get(path : Pathname.T) : TEXT RAISES 
  { OSError.E, Thread.Alerted, Rd.Failure };
  (* for quick-and-dirty code dealing with known-to-be-small files *)

PROCEDURE GetToWr(wr : Wr.T; path : Pathname.T) RAISES 
  { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure };

END FileUtils.
