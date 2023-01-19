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
  TE      = Text.Equal;

VAR
  Verbose := Debug.DebugThis("SpiceDecorate");

VAR
  emitted := NEW(TextSetDef.T).init();
  
PROCEDURE EnsureEmitted(typeNm : TEXT;
                        type   : SpiceCircuit.T) 
  RAISES { Wr.Failure } =
  BEGIN
    IF emitted.member(typeNm) THEN
      RETURN
    ELSE
      IF Verbose THEN
        Debug.Out("Emitting " & Debug.UnNil(typeNm));
      END;
      
      Emit(typeNm, type);
      
      IF typeNm # NIL THEN
        EVAL emitted.insert(typeNm)
      END
    END
  END EnsureEmitted;

PROCEDURE EmitElem(elem : SpiceObject.T; parent : SpiceCircuit.T)
  RAISES { Wr.Failure } =

  PROCEDURE Terminals() 
  RAISES { Wr.Failure } =
    BEGIN
      (*Wr.PutChar(wr, char);*) (* hmmm ... *)
      Wr.PutText(wr, elem.name);
      FOR i := 0 TO elem.terminals.size() - 1 DO
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, elem.terminals.get(i))
      END;
      Wr.PutChar(wr, ' ')
    END Terminals;

  PROCEDURE AddProbes() =
    BEGIN
      WITH myFullName = elem.name DO
        FOR i := 0 TO elem.terminals.size() - 1 DO
          WITH vp = F("v%s(%s)", Int(i + 1), myFullName),
               ip = F("i%s(%s)", Int(i + 1), myFullName) DO
            parent.probes.addhi(vp);
            parent.probes.addhi(ip)
          END
        END
      END
    END AddProbes;

  PROCEDURE T(txt : TEXT) 
  RAISES { Wr.Failure } =
    BEGIN
      Wr.PutText(wr, txt)
    END T;

  <*UNUSED*>
  PROCEDURE R(x : LONGREAL) 
  RAISES { Wr.Failure } =
    BEGIN
      Wr.PutText(wr, LR(x))
    END R;

  PROCEDURE V(v : SpiceObject.RealValue)
  RAISES { Wr.Failure } =
    BEGIN
      Wr.PutText(wr, SpiceObject.FmtReal(v))
    END V;


  PROCEDURE Data() 
  RAISES { Wr.Failure } =
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

  TYPE
    Literal = SpiceObject.RealLiteral;
    
  BEGIN
    IF Verbose THEN
      Debug.Out("EmitElem " & elem.name)
    END;
    
    TYPECASE elem OF
      SpiceObject.R(r) =>
      char := 'R'; Terminals(); V(r.r);
      
      addProbes := NOT ISTYPE(r.r, Literal) OR NARROW(r.r, Literal).v > minres
    |
      SpiceObject.C(c) =>
      char := 'C'; Terminals(); V(c.c);

      addProbes := NOT ISTYPE(c.c, Literal) OR NARROW(c.c, Literal).v > mincap
    |
      SpiceObject.L(l) =>
      char := 'L'; Terminals(); V(l.l)
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

    IF (char IN noprobe) AND
       NOT ((char = 'X' OR char = 'M') AND trantypes.member(type)) THEN
      (* always probe transistors *)
      
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
               type   : SpiceCircuit.T) 
  RAISES { Wr.Failure } =

  PROCEDURE Params() 
  RAISES { Wr.Failure } =
    BEGIN
      FOR i := 0 TO type.params.size() - 1 DO
        Wr.PutChar(wr, ' ');
        Wr.PutText(wr, type.params.get(i))
      END
    END Params;

  PROCEDURE Bindings() 
  RAISES { Wr.Failure } =
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

  PROCEDURE Probes() 
  RAISES { Wr.Failure } =
    BEGIN
      FOR i := 0 TO type.probes.size() - 1 DO
        Wr.PutText(wr, ".probe ");
        Wr.PutText(wr, type.probes.get(i));
        Wr.PutChar(wr, '\n')
      END
    END Probes;

  VAR
    stype : SpiceCircuit.T;
  BEGIN
    <*ASSERT type # NIL*>
    <*ASSERT typeNm # NIL*>

    IF Verbose THEN
      Debug.Out(F("Emit %s", typeNm))
    END;
    
    FOR i := 0 TO type.elements.size() - 1 DO
      WITH elem = type.elements.get(i) DO
        TYPECASE elem OF
          SpiceObject.X (x) =>

          IF Verbose THEN
            Debug.Out(F("Emit %s prereq : X object nm %s type %s", typeNm, x.name, x.type))
          END;

          WITH hadIt = spice.subCkts.get(x.type, stype) DO
            <*ASSERT hadIt*>
          END;

          IF doTransistorCkts OR NOT trantypes.member(x.type) THEN
            EnsureEmitted(x.type, stype)
          END
        ELSE
          (* skip *)
        END
      END
    END;

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

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  spiceFn    : Pathname.T     ;
  spice      : SpiceFormat.T;
  rootType   : TEXT := NIL;
  rootCkt    : SpiceCircuit.T;
  wr         : Wr.T;
  trantypes  := NEW(TextSetDef.T).init();
  mincap,
  minres     := 0.0d0;
  noprobe    := SET OF CHAR { };
  ofn        : Pathname.T := "-";
  doTransistorCkts : BOOLEAN;
  
BEGIN
  TRY
    IF pp.keywordPresent("-i") THEN
      spiceFn := pp.getNext()
    END;
    IF pp.keywordPresent("-root") THEN
      rootType := pp.getNext()
    END;
    IF pp.keywordPresent("-o") THEN
      ofn := pp.getNext()
    END;
    IF pp.keywordPresent("-mincap") THEN
      mincap := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-minres") THEN
      minres := pp.getNextLongReal()
    END;

    doTransistorCkts := pp.keywordPresent("-transistorckts");

    WHILE pp.keywordPresent("-n") DO
      WITH arg  = pp.getNext(),
           char = Text.GetChar(arg, 0) DO
        noprobe := noprobe + SET OF CHAR { char }
      END
    END;
    
    IF pp.keywordPresent("-t") THEN
      WITH fn = pp.getNext() DO
        TRY
          WITH   rd = FileRd.Open(fn) DO
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
        EXCEPT
          Rd.Failure(x) =>
          Debug.Error(F("Reading transistor types file %s : Rd.Failure : %s",
                        fn, AL.Format(x)))
        |
          OSError.E(x) =>
          Debug.Error(F("Can't open transistor types file %s : OSError.E : %s",
                        fn, AL.Format(x)))
        END
      END        
    END;
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF TE(ofn, "-") THEN
    wr := Stdio.stdout
  ELSE
    TRY
      wr := FileWr.Open(ofn)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Can't open output %s for writing : OSError.E : %s",
                    ofn, AL.Format(x)))
    END
  END;

  TRY
  Wr.PutText(wr, "* Generated by SpiceDecorate.m3 @ " & LR(Time.Now()) & "\n");

  Wr.PutText(wr, "* ");
  FOR i := 0 TO Params.Count - 1 DO
    Wr.PutText(wr, " ");
    Wr.PutText(wr, Params.Get(i));
  END;
  Wr.PutText(wr, "\n");
  
  TRY
    WITH spiceRd = FileRd.Open(spiceFn) DO
      spice := SpiceFormat.ParseSpice(spiceRd, ".", spiceFn);
      Rd.Close(spiceRd)
    END
  EXCEPT
    Rd.Failure(x) =>
    Debug.Error(F("Reading spice input %s : Rd.Failure : %s",
                  spiceFn, AL.Format(x)))
  |
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

  EnsureEmitted(rootType, rootCkt);

  Wr.Close(wr)
EXCEPT
  Wr.Failure(x) =>
         Debug.Error(F("Can't write output : Wr.Failure : %s",
                        AL.Format(x)))
END

END SpiceDecorate.
