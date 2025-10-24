(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Params;
IMPORT Rd, FileRd;
IMPORT Debug;
IMPORT FileWr, Wr;
IMPORT SpiceCircuit;
IMPORT SpiceFormat;
IMPORT TextTextSetTbl;
IMPORT OSError;
IMPORT SpiceError;
FROM Fmt IMPORT F, Int;
IMPORT TextRefTbl;
IMPORT TextTextTbl;
IMPORT SpiceInstance;
IMPORT TextSpiceInstanceSetTbl;
IMPORT FlatUI;
IMPORT AL;

FROM SpiceFlat IMPORT Visit, DumpOneType, DumpGprofFormat, DumpBriefFlat,
                      VisitCktNodes, DumpSymtab, CleanAssocs;

VAR
  fn                       := Params.Get(1);
  rd     : Rd.T;
  top                      := Params.Get(2);

  topCkt : SpiceCircuit.T;
  spice  : SpiceFormat.T;
BEGIN
  TRY
    rd    := FileRd.Open(fn);
    spice := SpiceFormat.ParseSpice(rd, ".", fn);
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                  fn, AL.Format(e)))
  |
    SpiceError.E(e) =>
    Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                  e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
  END;

  IF NOT spice.subCkts.get(top, topCkt) THEN
    Debug.Error("Can't find top-level subcircuit def'n \"" & top & "\"")
  END;
  
  WITH wr = FileWr.Open("flat.out") DO
    Visit("TOP", wr, topCkt, spice.subCkts);
    Wr.Close(wr)
  END;

  VAR 
    wr         := FileWr.Open("hier.out");
    iter       := spice.subCkts.iterate();
    typeCntTbl := NEW(TextRefTbl.Default).init();
    parentTbl  := NEW(TextTextSetTbl.Default).init();
    gwr        := FileWr.Open("gprof.out");
    bwr        := FileWr.Open("bflat.out");

    type  : TEXT;
    ckt   : SpiceCircuit.T;
    
  BEGIN
    WHILE iter.next(type, ckt) DO
      DumpOneType(wr, type, ckt, typeCntTbl, parentTbl)
    END;
    Wr.Close(wr);

    DumpGprofFormat(gwr, typeCntTbl, parentTbl);
    Wr.Close(gwr);

    DumpBriefFlat(bwr, top, typeCntTbl);
    Wr.Close(bwr)
  END;

  (* print out all the aliases ... *)
  VAR
    topName := "X1"; (* s.b. cmd-line param *)
    symTab := NEW(TextTextSetTbl.Default).init();
    assocs := NEW(TextSpiceInstanceSetTbl.Default).init();
    topInstance := NEW(SpiceInstance.T).init("X1", NIL (* not right *), NIL);
    canonTbl := NEW(TextTextTbl.Default).init();
  BEGIN
    VisitCktNodes(topName, symTab, topCkt, NIL, assocs, topInstance, spice.subCkts);
    WITH wr = FileWr.Open("aliases.txt") DO
      DumpSymtab(wr, symTab, canonTbl);
      Wr.Close(wr)
    END;

    (* clean up assocs, merging any unmerged aliases *)
    assocs := CleanAssocs(assocs, canonTbl);

    FlatUI.REPL(assocs, symTab, canonTbl)
  END

END Main.
