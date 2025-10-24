(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT DataBlock;
IMPORT ParseParams;
IMPORT FileRd;

IMPORT OSError;
IMPORT Debug;
IMPORT Rd;
IMPORT AL;
IMPORT Stdio;
IMPORT Pathname;
IMPORT Params;
IMPORT Text;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST
  TE = Text.Equal;

CONST
  Usage = "-|<filename>";
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  ifn : Pathname.T := NIL;

  rd : Rd.T := NIL;
  
BEGIN
  TRY
    pp.skipParsed();

    ifn := pp.getNext();

    pp.finish();

      EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF TE (ifn, "-") THEN
    rd := Stdio.stdin
  ELSE
    TRY
      rd := FileRd.Open(ifn)
    EXCEPT
      OSError.E(x) => Debug.Error("Trouble opening input file \"" & ifn & "\": OSError.E : " & AL.Format(x))
    END
  END;

  TRY
    DataBlock.DebugTraverse(rd, ifn)
  EXCEPT
    Rd.Failure(x) => Debug.Error("DataBlock.DebugTraverse raised Rd.Failure : " & AL.Format(x))
  END;

  TRY
    Rd.Close(rd)
  EXCEPT
    Rd.Failure(x) => Debug.Error("Trouble closing input file \"" & ifn & "\": Rd.Failure : " & AL.Format(x))
  END
END Main.
