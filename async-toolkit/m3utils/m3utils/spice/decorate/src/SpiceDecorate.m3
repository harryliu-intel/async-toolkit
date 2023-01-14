MODULE SpiceDecorate EXPORTS Main;
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
IMPORT Wr;
IMPORT FileWr;
IMPORT Thread;
IMPORT TextSetDef;
IMPORT Time;
IMPORT Text;

<*FATAL Thread.Alerted*>

CONST
  Usage   = "";
  LR      = LongReal;

VAR
  Verbose := TRUE;

VAR
  emitted := NEW(TextSetDef.T).init();
  
PROCEDURE EnsureEmitted(typeNm : TEXT;
                        type   : SpiceCircuit.T) =
  BEGIN
    IF emitted.member(typeNm) THEN
      RETURN
    ELSE
      Debug.Out("Emitting " & Debug.UnNil(typeNm));

      Emit(typeNm, type);
      
      IF typeNm # NIL THEN
        EVAL emitted.insert(typeNm)
      END
    END
  END EnsureEmitted;

PROCEDURE EmitElem(elem : SpiceObject.T; parent : SpiceCircuit.T) =

  PROCEDURE Terminals() =
    BEGIN
      Wr.PutChar(wr, char);
      Wr.PutText(wr, elem.name);
      FOR i := 0 TO elem.terminals.size() - 1 DO
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, elem.terminals.get(i))
      END;
      Wr.PutChar(wr, ' ')
    END Terminals;

  PROCEDURE AddProbes() =
    BEGIN
      WITH myFullName = Text.FromChar(char) & elem.name DO
        FOR i := 0 TO elem.terminals.size() - 1 DO
          WITH vp = F("v%s(%s)", Int(i + 1), myFullName),
               ip = F("i%s(%s)", Int(i + 1), myFullName) DO
            parent.probes.addhi(vp);
            parent.probes.addhi(ip)
          END
        END
      END
    END AddProbes;

  PROCEDURE T(txt : TEXT) =
    BEGIN
      Wr.PutText(wr, txt)
    END T;

  PROCEDURE R(x : LONGREAL) =
    BEGIN
      Wr.PutText(wr, LR(x))
    END R;


  PROCEDURE Data() =
    BEGIN
      FOR i := 0 TO elem.data.size() - 1 DO
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, elem.data.get(i))
      END
    END Data;
    
  VAR
    char : CHAR;
    addProbes := TRUE;
    type := "";
    
  BEGIN
    Debug.Out("EmitElem " & elem.name);

    TYPECASE elem OF
      SpiceObject.R(r) =>
      char := 'R'; Terminals(); R(r.r);
      
      addProbes := r.r > minres
    |
      SpiceObject.C(c) =>
      char := 'C'; Terminals(); R(c.c);

      addProbes := c.c > mincap
    |
      SpiceObject.L(l) =>
      char := 'L'; Terminals(); R(l.l)
    |
      SpiceObject.X(x) =>
      char := 'X'; Terminals(); T(x.type); type := x.type
    |
      SpiceObject.M(m) =>
      char := 'M'; Terminals(); T(m.type); type := m.type
    |
      SpiceObject.D(d) =>
      char := 'D'; Terminals(); T(d.type); type := d.type
    ELSE
      <*ASSERT FALSE*>
    END;

    IF (char IN noprobe) AND NOT trantypes.member(type) THEN
      IF FALSE THEN
        Debug.Out(F("char %s type %s, skipping probe",
                    Text.FromChar(char), type));
      END;
      
      addProbes := FALSE
    END;
    
    IF addProbes THEN
      AddProbes()
    END;
    
    Data();

    Wr.PutChar(wr, '\n')
  END EmitElem;

PROCEDURE Emit(typeNm : TEXT;
               type   : SpiceCircuit.T) =

  PROCEDURE Params() =
    BEGIN
      FOR i := 0 TO type.params.size() - 1 DO
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, type.params.get(i))
      END
    END Params;

  PROCEDURE Bindings() =
    VAR
      k, v : TEXT;
    BEGIN
      IF type.bindings # NIL THEN
        WITH iter = type.bindings.iterate() DO
          WHILE iter.next(k, v) DO
            Wr.PutText(wr, F(" %s=%s", k, v))
          END
        END
      END
    END Bindings;

  PROCEDURE Probes() =
    BEGIN
      FOR i := 0 TO type.probes.size() - 1 DO
        Wr.PutText(wr, ".probe ");
        Wr.PutText(wr, type.probes.get(i));
        Wr.PutChar(wr, '\n')
      END
    END Probes;
    
  BEGIN
    <*ASSERT type # NIL*>
    <*ASSERT typeNm # NIL*>
    
    Wr.PutText(wr, ".SUBCKT ");
    Wr.PutText(wr, typeNm);
    Params();
    Bindings();
    Wr.PutChar(wr, '\n');
    
    <*ASSERT type.elements # NIL*>
    FOR i := 0 TO type.elements.size() - 1 DO
      WITH elem = type.elements.get(i) DO
        EmitElem(elem, type)
      END
    END;
    Probes();
    Wr.PutText(wr, F(".ENDS %s\n\n", typeNm));
  END Emit;

PROCEDURE DoIt(ckt  : SpiceCircuit.T;
               path : TEXT) =
  VAR
    elems := ckt.elements;
    type : SpiceCircuit.T;
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

          IF NOT trantypes.member(x.type) THEN
            EnsureEmitted(x.type, type)
          END;
          
          DoIt(type, path & x.name & ".")
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
  wr         := FileWr.Open("decorated.sp");
  trantypes  := NEW(TextSetDef.T).init();
  mincap,
  minres     := 0.0d0;
  noprobe    := SET OF CHAR { };
  
BEGIN
  Wr.PutText(wr, "* Generated by SpiceDecorate.m3 @ " & LR(Time.Now()) & "\n");
  
  TRY
    IF pp.keywordPresent("-i") THEN
      spiceFn := pp.getNext()
    END;
    IF pp.keywordPresent("-root") THEN
      rootType := pp.getNext()
    END;
    IF pp.keywordPresent("-mincap") THEN
      mincap := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-minres") THEN
      minres := pp.getNextLongReal()
    END;

    WHILE pp.keywordPresent("-n") DO
      WITH arg  = pp.getNext(),
           char = Text.GetChar(arg, 0) DO
        noprobe := noprobe + SET OF CHAR { char }
      END
    END;
    
    IF pp.keywordPresent("-t") THEN
      WITH fn = pp.getNext(),
           rd = FileRd.Open(fn) DO
        TRY
          LOOP
            WITH line = Rd.GetLine(rd) DO
              EVAL trantypes.insert(line)
            END
          END
        EXCEPT
          Rd.EndOfFile => Rd.Close(rd)
        END
      END
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

  DoIt(rootCkt, "");

  EnsureEmitted(rootType, rootCkt);

  Wr.Close(wr)

END SpiceDecorate.
