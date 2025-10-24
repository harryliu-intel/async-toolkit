(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpiceUncompress EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Trace;
IMPORT CardSeq;
IMPORT Debug;
IMPORT Params;
IMPORT UnsafeWriter;
IMPORT FS;
IMPORT TraceFile;
IMPORT FileWr;
IMPORT Wr;
IMPORT Time;
FROM Fmt IMPORT F, Int;
IMPORT RegEx, RegExList;
IMPORT TextSet;
IMPORT Thread;
IMPORT NameControl;
IMPORT TextArraySort;

<*FATAL Thread.Alerted*>

PROCEDURE Map(READONLY ia : ARRAY OF LONGREAL;
              iseq        : CardSeq.T;
              VAR      oa : ARRAY OF LONGREAL) =
  BEGIN
    FOR i := 0 TO iseq.size() - 1 DO
      oa[i] := ia[iseq.get(i)]
    END
  END Map;

CONST
  Usage = " [-minstep <minstep> ] <rootname>";
  
PROCEDURE HaveMatch(rx : RegExList.T; aliases : TextSet.T) : BOOLEAN =
  VAR
    iter := aliases.iterate();
    a : TEXT;
  BEGIN
    WHILE iter.next(a) DO
      VAR
        p := rx;
      BEGIN
        WHILE p # NIL DO
          IF RegEx.Execute(p.head, a) # -1 THEN RETURN TRUE END;
          p := p.tail
        END
      END
    END;
    RETURN FALSE
  END HaveMatch;
  
PROCEDURE PutNames(nwr : Wr.T; aliases : TextSet.T) =
  VAR
    iter  := aliases.iterate();
    first := TRUE;
    a : TEXT;
    arr := NEW(REF ARRAY OF TEXT, aliases.size());
    i := 0;
  BEGIN
    WHILE iter.next(a) DO arr[i] := a; INC(i) END;

    IF sortNames THEN
      TextArraySort.Sort(arr^, cmp := NameControl.CompareText)
    END;

    FOR i := FIRST(arr^) TO LAST(arr^) DO
      IF i # FIRST(arr^) THEN
        Wr.PutChar(nwr, '=')
      END;
      Wr.PutText(nwr, arr[i])
    END;
    Wr.PutChar(nwr, '\n')
  END PutNames;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  minStep := 0.0d0;
  ibuff, obuff : REF ARRAY OF LONGREAL;

  root, oroot : TEXT := NIL;
  trace : Trace.T;
  active := NEW(CardSeq.T).init();

  regExList : RegExList.T := NIL;
  noNames : BOOLEAN;
  sortNames : BOOLEAN;
  nwr : Wr.T := NIL;
BEGIN
  TRY
    noNames := pp.keywordPresent("-nonames");
    sortNames := pp.keywordPresent("-sortnames");
    
    IF pp.keywordPresent("-minstep") THEN
      minStep := pp.getNextLongReal()
    END;

    WHILE pp.keywordPresent("-r") DO
      WITH pat = pp.getNext() DO
        regExList := RegExList.Cons(RegEx.Compile(pat), regExList)
      END
    END;

    IF pp.keywordPresent("-o") THEN
      oroot := pp.getNext()
    END;
    
    pp.skipParsed();

    root := pp.getNext();

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF oroot = NIL THEN
    Debug.Error("Must specify -o")
  END;
  
  TRY
    FS.DeleteFile(oroot & ".trace")
  EXCEPT
  ELSE
  END;

  trace := NEW(Trace.T).init(root);

  Debug.Out(F("steps %s", Int(trace.getSteps())));
  ibuff := NEW(REF ARRAY OF LONGREAL, trace.getSteps());

  VAR
    p : LONGREAL;
  BEGIN
    trace.getTimeData(ibuff^);
    p := ibuff[0];
    active.addhi(0);
    
    FOR i := 1 TO LAST(ibuff^) DO
      IF ibuff[i] >= p + minStep THEN
        p := ibuff[i];
        active.addhi(i)
      END
    END
  END;

  obuff := NEW(REF ARRAY OF LONGREAL, active.size());

  VAR
    theNodes : CardSeq.T := NIL;
    nNodes : CARDINAL;
  BEGIN
    (* if we are demanding regex matching, we need to check which nodes 
       match, and how many there are *)
    IF regExList = NIL THEN
      nNodes := trace.getNodes();
    ELSE
      theNodes := NEW(CardSeq.T).init();
      FOR ni := 0 TO trace.getNodes() - 1 DO
        WITH aliases = trace.getAliases(ni) DO
          IF HaveMatch(regExList, aliases) THEN
            Debug.Out("matching node " & Int(ni));
            theNodes.addhi(ni)
          END
        END
      END;
      nNodes := theNodes.size()
    END;
  
    WITH header = TraceFile.Header { TraceFile.Version.Reordered,
                                     Time.Now(),
                                     nNodes },
         wr     = FileWr.Open(oroot & ".trace") DO
      
      IF NOT noNames THEN
        nwr    := FileWr.Open(oroot & ".names")
      END;
      
      TraceFile.WriteHeader(wr, header);

      IF regExList = NIL THEN
        FOR ni := 0 TO trace.getNodes() - 1 DO
          trace.getNodeData(ni, ibuff^);
          Map(ibuff^, active, obuff^);
          UnsafeWriter.WriteLRA(wr, obuff^);
          IF nwr # NIL THEN PutNames(nwr, trace.getAliases(ni)) END
        END
      ELSE
        FOR p := 0 TO theNodes.size() - 1 DO
          WITH ni = theNodes.get(p) DO
            Debug.Out("dumping data for node " & Int(ni));
            trace.getNodeData(ni, ibuff^);
            Map(ibuff^, active, obuff^);
            UnsafeWriter.WriteLRA(wr, obuff^);
            IF nwr # NIL THEN PutNames(nwr, trace.getAliases(ni)) END
          END
        END
      END;
      Wr.Close(wr);
    END;

    IF nwr # NIL THEN Wr.Close(nwr) END
  END
END SpiceUncompress.
