MODULE ListCaps EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT Pathname;
IMPORT SpiceFormat;
IMPORT SpiceError;
IMPORT AL;
IMPORT Rd, FileRd;
IMPORT OSError;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT Comp AS Cap, CompSeq AS CapSeq, CompArraySort AS CapArraySort;
IMPORT TextCompTbl AS TextCapTbl;
IMPORT Wr;
IMPORT FileWr;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST
  Usage   = "";
  LR      = LongReal;

VAR
  Verbose := TRUE;

PROCEDURE DoIt(ckt : SpiceCircuit.T;
               path : TEXT;
               caps : CapSeq.T;
               hier : TextCapTbl.T) =
  VAR
    elems := ckt.elements;
    type : SpiceCircuit.T;
    cap  : Cap.T;
  BEGIN
    FOR i := 0 TO elems.size() - 1 DO
      
      WITH elem = elems.get(i) DO
        TYPECASE elem OF
          SpiceObject.X (x) =>

          IF Verbose AND FALSE THEN
            Debug.Out(F("X object nm %s type %s", x.name, x.type))
          END;

          WITH hadIt = spice.subCkts.get(x.type, type) DO
            <*ASSERT hadIt*>
          END;

          DoIt(type, path & x.name & ".", caps, hier)
        |
          SpiceObject.C(c) =>
          <*ASSERT c # NIL*>
          <*ASSERT caps # NIL*>
          <*ASSERT c.name # NIL*>
          <*ASSERT path # NIL*>
          caps.addhi(Cap.T { path & c.name, c.c });
          
          WITH hnm = ckt.name & "/" & c.name DO
            IF NOT hier.get(hnm, cap) THEN
              cap := Cap.T { hnm, c.c, 0 }
            END;
            INC(cap.m);
            EVAL hier.put(hnm, cap)
          END
        ELSE
          (* skip *)
        END
      END
    END
          
  END DoIt;
  
VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  spiceFn    : Pathname.T     ;
  spice      : SpiceFormat.T;
  rootType   : TEXT := NIL;
  rootCkt    : SpiceCircuit.T;
  caps       : CapSeq.T;
  sum        := 0.0d0;
  hier       := NEW(TextCapTbl.Default).init();
BEGIN
  TRY
    IF pp.keywordPresent("-i") THEN
      spiceFn := pp.getNext()
    END;
    IF pp.keywordPresent("-root") THEN
      rootType := pp.getNext()
    END;
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    WITH spiceRd = FileRd.Open(spiceFn) DO
      spice := SpiceFormat.ParseSpice(spiceRd, ".", spiceFn);
      Rd.Close(spiceRd)
    END
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                  spiceFn, AL.Format(e)))
  |
    SpiceError.E(e) =>
    Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                  e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
  END;
  
  IF rootType = NIL THEN
    rootCkt := spice.topCkt
  ELSE
    WITH hadIt = spice.subCkts.get(rootType, rootCkt) DO
      IF NOT hadIt THEN
        Debug.Error(F("Unknown root type %s", rootType))
      END
    END
  END;

  Debug.Out("Done parsing.");

  caps := NEW(CapSeq.T).init();
  hier := NEW(TextCapTbl.Default).init();
  
  DoIt(rootCkt, "", caps, hier);

  WITH a = NEW(REF ARRAY OF Cap.T, caps.size()),
       flatWr = FileWr.Open("cap_flat.dat"),
       hierWr = FileWr.Open("cap_hier.dat"),
       singWr = FileWr.Open("cap_sing.dat")
    DO
    FOR i := 0 TO caps.size() - 1 DO
      a[i] := caps.get(i)
    END;
    CapArraySort.Sort(a^);
    Debug.Out("writing flat");
    FOR i := FIRST(a^) TO LAST(a^) DO
      sum := sum + a[i].val;
      Wr.PutText(flatWr, F("%-15s %s\n", LR(a[i].val), a[i].nm))
    END;

    Debug.Out(F("%s capacitors, total cap %s, smallest %s, largest %s",
                Int(caps.size()), LR(sum), LR(a[0].val), LR(a[LAST(a^)].val)));

    VAR
      i := 0 ;
      txt : TEXT;
      totM := 0;
      iter := hier.iterate();

      cap : Cap.T;
      a := NEW(REF ARRAY OF Cap.T, hier.size());
    BEGIN
      WHILE iter.next(txt, cap) DO
        a[i] := cap;
        INC(i)
      END;

      CapArraySort.Sort(a^);
      Debug.Out("writing sing");
      FOR i := FIRST(a^) TO LAST(a^) DO
        Wr.PutText(singWr, F("%-15s %10s %-15s %s\n",
                             LR(a[i].val),
                             Int(a[i].m),
                             LR(a[i].val * FLOAT(a[i].m, LONGREAL)),
                             a[i].nm))
      END;

      CapArraySort.Sort(a^, cmp := Cap.CompareByMulVal);
      Debug.Out("writing hier");
      FOR i := FIRST(a^) TO LAST(a^) DO
        Wr.PutText(hierWr, F("%-15s %10s %-15s %s\n",
                             LR(a[i].val),
                             Int(a[i].m),
                             LR(a[i].val * FLOAT(a[i].m, LONGREAL)),
                             a[i].nm));
        INC(totM, a[i].m);
      END;
      Debug.Out(F("Tot hier %s tot m %s", Int(NUMBER(a^)), Int(totM)));

    END;

    
    Wr.Close(flatWr);
    Wr.Close(hierWr);

    
  END
END ListCaps.
