(*                                                                           *)
(*  Debug.m3                                                                 *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id$ *)

MODULE Debug;
IMPORT Wr;
FROM Stdio IMPORT stderr;

EXCEPTION
  ABORT;

PROCEDURE Out(t: TEXT; minLevel : CARDINAL) =
  BEGIN
    IF minLevel > level THEN RETURN END;

    TRY
      Wr.PutText(stderr,t & "\n");
      Wr.Flush(stderr);
    EXCEPT ELSE END;
  END Out;

PROCEDURE Warning(t: TEXT) =
  BEGIN
    TRY
      Wr.PutText(stderr,"WARNING: " & t & "\n");
      Wr.Flush(stderr);
    EXCEPT ELSE END;
  END Warning;

PROCEDURE Error(t: TEXT) =
<*FATAL ABORT*>
  BEGIN
    TRY
      Wr.PutText(stderr,"ERROR: " & t & "\n");
      Wr.Flush(stderr);
    EXCEPT ELSE END;
    RAISE ABORT;
  END Error;

PROCEDURE UnNil(text : TEXT) : TEXT =
  BEGIN IF text = NIL THEN RETURN "(NIL)" ELSE RETURN text END END UnNil;

BEGIN level := 0 END Debug.
