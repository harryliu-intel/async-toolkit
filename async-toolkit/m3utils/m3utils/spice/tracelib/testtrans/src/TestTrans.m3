(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TestTrans EXPORTS Main;

IMPORT Trace;
IMPORT Transition;
IMPORT TransitionFinder;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Stdio;
FROM Fmt IMPORT LongReal, Int, F;
IMPORT Wr;
IMPORT Params;

CONST Usage = "";
      LR    = LongReal;
VAR
  pp            := NEW(ParseParams.T).init(Stdio.stderr);
  tr         : Trace.T;
  root       : TEXT;
  thresh     : LONGREAL;
  hyster     : LONGREAL;
  nodeId     : Trace.NodeId;
BEGIN
  TRY
    IF pp.keywordPresent("-root") THEN    
      root := pp.getNext()
    END;
    IF pp.keywordPresent("-n") THEN
      nodeId := pp.getNextInt()
    END;
    pp.skipParsed();

    thresh := pp.getNextLongReal();
    hyster := pp.getNextLongReal();

    pp.finish()
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  tr := NEW(Trace.T).init(root);

  WITH time = NEW(REF ARRAY OF LONGREAL, tr.getSteps()),
       data = NEW(REF ARRAY OF LONGREAL, tr.getSteps()) DO
    tr.getTimeData(time^);
    tr.getNodeData(nodeId, data^);

    WITH seq = TransitionFinder.Find(time^, data^, thresh, hyster, TRUE) DO
      FOR i := 0 TO seq.size() - 1 DO
        Wr.PutText(Stdio.stdout, Transition.Format(seq.get(i)));
        Wr.PutChar(Stdio.stdout, '\n');
        Wr.Flush  (Stdio.stdout)
      END
    END
  END
END TestTrans.
