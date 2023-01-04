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
FROM Fmt IMPORT F, Int, LongReal, Bool;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT Text;
IMPORT CitTextUtils;
IMPORT Comp AS Cap, CompSeq AS CapSeq, CompArraySort AS CapArraySort;
IMPORT Wr;

CONST
  Usage   = "";
  LR      = LongReal;

VAR
  Verbose := TRUE;

PROCEDURE DoIt(ckt : SpiceCircuit.T; path : TEXT; caps : CapSeq.T) =
  VAR
    elems := ckt.elements;
    type : SpiceCircuit.T;
  BEGIN
    FOR i := 0 TO elems.size() - 1 DO
      
      WITH elem = elems.get(i) DO
        TYPECASE elem OF
          SpiceObject.X (x) =>

          IF Verbose THEN
            Debug.Out(F("X object nm %s type %s", x.name, x.type))
          END;

          WITH hadIt = spice.subCkts.get(x.type, type) DO
            <*ASSERT hadIt*>
          END;

          DoIt(type, path & x.name & ".", caps)
        |
          SpiceObject.C(c) =>
          <*ASSERT c # NIL*>
          <*ASSERT caps # NIL*>
          <*ASSERT c.name # NIL*>
          <*ASSERT path # NIL*>
          caps.addhi(Cap.T { path & c.name, c.c })
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
  
  DoIt(rootCkt, "", caps);

  WITH a = NEW(REF ARRAY OF Cap.T, caps.size()) DO
    FOR i := 0 TO caps.size() - 1 DO
      a[i] := caps.get(i)
    END;
    CapArraySort.Sort(a^);
    FOR i := FIRST(a^) TO LAST(a^) DO
      sum := sum + a[i].val;
      Wr.PutText(Stdio.stdout, F("%-15s %s\n", LR(a[i].val), a[i].nm))
    END;

    Debug.Out(F("%s capacitors, total cap %s, smallest %s, largest %s",
                Int(caps.size()), LR(sum), LR(a[0].val), LR(a[LAST(a^)].val)))
  END
END ListCaps.
