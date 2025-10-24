(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE FingerprintFinder;

IMPORT Pathname, OSError, Rd;
IMPORT FileRd;
IMPORT TextReader;
IMPORT Text;
IMPORT Thread;
IMPORT Debug;

<*FATAL Thread.Alerted*>

CONST doDebug = TRUE;

PROCEDURE Find(pn : Pathname.T; key : TEXT) : TEXT =
  VAR
    rd   : Rd.T;
    word : TEXT;
  BEGIN
    IF key = NIL THEN key := DefKey END;
    
    TRY
      IF doDebug THEN Debug.Out("Find opening " & pn) END;
      
      rd := FileRd.Open(pn);
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line),
             haveOne = reader.next(" ", word) DO

          IF doDebug THEN Debug.Out("Line " & line) END;
          
          IF haveOne AND Text.Equal(word, key) THEN
            RETURN reader.nextE("")
          END
        END
      END
    EXCEPT
      Rd.Failure, TextReader.NoMore, OSError.E, Rd.EndOfFile =>
      (* note that catching ANY exception here (with ELSE) also catches
         RETURN... *)
      RETURN NIL
    END
  END Find;

BEGIN END FingerprintFinder.
  
