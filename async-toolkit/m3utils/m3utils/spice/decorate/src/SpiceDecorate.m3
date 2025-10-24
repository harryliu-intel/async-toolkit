MODULE SpiceDecorate EXPORTS Main;

(*

  this program manipulates spice decks in various ways...

*)

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
IMPORT CharTextTbl;
IMPORT TextSeq;
IMPORT Scheme;
IMPORT SchemeM3;
IMPORT SchemeSymbol;
IMPORT SchemeStubs;
IMPORT RTBrand;
IMPORT RegEx, RegExList;

<*FATAL Thread.Alerted*>

CONST
  Usage   = "-i <input filename> -root <root type> [-o (<output filename>|-)] [-mincap <min cap>] [-minres <min res>] [-transistorckts] [[-n <type-not-to-probe>]...] [-t <transistor type file name>]";
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

PROCEDURE ModifyElem(elem         : SpiceObject.T;
                     parent       : SpiceCircuit.T;
                     modification : Modification) : SpiceObject.T =
  VAR
    char   : CHAR;
    modScm : TEXT;
  BEGIN
    TYPECASE elem OF
      SpiceObject.R =>
      char := 'R'
    |
      SpiceObject.C =>
      char := 'C'
    |
      SpiceObject.L =>
      char := 'L'
    |
      SpiceObject.X =>
      char := 'X'
    |
      SpiceObject.M =>
      char := 'M'
    |
      SpiceObject.D =>
      char := 'D'
    ELSE
      <*ASSERT FALSE*>
    END;

    IF modification.modifiers # NIL              AND
       modification.modifiers.get(char, modScm)          THEN
      scm.defineInGlobalEnv(SchemeSymbol.FromText("the-spice-object"), elem);
      scm.defineInGlobalEnv(SchemeSymbol.FromText("the-spice-parent"), parent);
      TRY
        WITH res = scm.loadEvalText(modScm) DO
          IF ISTYPE(res, SpiceObject.T) THEN
            RETURN res
          ELSE
            Debug.Error(F("?error : %s returns wrong type : %s",
                          modScm, RTBrand.GetName(TYPECODE(res))))
          END
        END
      EXCEPT
        Scheme.E(err) =>
        Debug.Error(F("?error modifying element \"%s\" : Scheme.E : %s",
                      SpiceObject.Format(elem),
                      err));
        <*ASSERT FALSE*>
      END
    ELSE
      RETURN elem
    END
  END ModifyElem;
  
PROCEDURE EmitElem(elem         : SpiceObject.T;
                   parent       : SpiceCircuit.T;
                   modification : Modification
                   )
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

  PROCEDURE V(v : SpiceObject.RealValue; mul : LONGREAL)
  RAISES { Wr.Failure } =
    BEGIN
      Wr.PutText(wr, SpiceObject.FmtReal(v, mul))
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
    addProbes := NOT noProbes;
    type := "";

  TYPE
    Literal = SpiceObject.RealLiteral;
    
  BEGIN
    IF Verbose THEN
      Debug.Out("EmitElem " & elem.name)
    END;

    TYPECASE elem OF
      SpiceObject.R(r) =>
      char := 'R'; Terminals(); V(r.r, modification.resmul);
      
      addProbes := addProbes AND (NOT ISTYPE(r.r, Literal) OR NARROW(r.r, Literal).v > minres)
    |
      SpiceObject.C(c) =>
      char := 'C'; Terminals(); V(c.c, modification.capmul);

      addProbes := addProbes AND (NOT ISTYPE(c.c, Literal) OR NARROW(c.c, Literal).v > mincap)
    |
      SpiceObject.L(l) =>
      char := 'L'; Terminals(); V(l.l, modification.indmul)
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
      END;

      IF probeSubckts THEN
        FOR i := 0 TO type.params.size() - 1 DO
          Wr.PutText(wr, ".probe v(");
          Wr.PutText(wr, type.params.get(i));
          Wr.PutText(wr, ")\n");

        END
      END;

      IF typeProbes THEN
        FOR i := 0 TO type.params.size() - 1 DO
          WITH untypedName = type.params.get(i),
               typedName   = probePfx & type.name & "_" & untypedName DO
            Wr.PutText(wr, F("V%s %s %s DC 0\n",
                             typedName, untypedName, typedName)
            );
            Wr.PutText(wr, ".probe v(");
            Wr.PutText(wr, typedName);
            Wr.PutText(wr, ")\n");
          END
        END
      END

    END Probes;

  VAR
    stype        : SpiceCircuit.T;
    modification : Modification;
  BEGIN
    <*ASSERT type # NIL*>
    <*ASSERT typeNm # NIL*>

    IF Verbose THEN
      Debug.Out(F("Emit %s", typeNm))
    END;

    (* see if we are restricting the modTypes, and if so, if the type
       we are processing is on the modification list *)
    IF modTypes = NIL THEN
      modification := stdModification
    ELSE
      modification := NoModification;
      VAR
        p := modTypes;
      BEGIN
        WHILE p # NIL DO
          IF RegEx.Execute(p.head, typeNm) # -1 THEN
            modification := stdModification;
            EXIT
          END;
          p := modTypes.tail
        END
      END
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
        WITH modElem = ModifyElem(elem, type, modification) DO
          EmitElem(modElem, type, modification)
        END
      END
    END;
    Probes();
    Wr.PutText(wr, F(".ENDS %s\n\n", typeNm));
  END Emit;

TYPE
  Modification = RECORD
    capmul, resmul, indmul : LONGREAL;
    modifiers              : CharTextTbl.T;
  END;

VAR
  NoModification := Modification { 1.0d0, 1.0d0, 1.0d0, NIL };

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);
  spiceFn    : Pathname.T     ;
  spice      : SpiceFormat.T;
  rootType   : TEXT := NIL;
  rootCkt    : SpiceCircuit.T;
  wr         : Wr.T;
  trantypes                   := NEW(TextSetDef.T).init();
  mincap,
  minres                      := 0.0d0;
  noprobe                     := SET OF CHAR { };
  ofn        : Pathname.T     := "-";
  doTransistorCkts : BOOLEAN;
  probeSubckts : BOOLEAN;
  typeProbes   : BOOLEAN;
  probePfx                    := "";
  noProbes                    := FALSE;
  scmFiles                    := NEW(TextSeq.T).init();
  scm      : Scheme.T;

  stdModification             := NoModification;

  modTypes : RegExList.T      := NIL;
  
BEGIN
  TRY

    stdModification.modifiers := NEW(CharTextTbl.Default).init();

    probeSubckts := pp.keywordPresent("-probesubckts");

    typeProbes := pp.keywordPresent("-typeprobesubckts");

    IF typeProbes THEN
      IF pp.keywordPresent("-probeprefix") THEN
        probePfx := pp.getNext()
      END
    END;
    
    IF pp.keywordPresent("-i") THEN
      spiceFn := pp.getNext()
    ELSE
      Debug.Error("?error : must provide -i <input file name>")
    END;
    IF pp.keywordPresent("-root") THEN
      rootType := pp.getNext()
    ELSE
      Debug.Error("?error : must provide -root <root type>")
    END;
    IF pp.keywordPresent("-noprobe") THEN
      noProbes := TRUE
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
    IF pp.keywordPresent("-capmul") THEN
      stdModification.capmul := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-resmul") THEN
      stdModification.resmul := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-indmul") THEN
      stdModification.indmul := pp.getNextLongReal()
    END;

    WHILE pp.keywordPresent("-modregex") DO
      WITH regExStr = pp.getNext() DO
        TRY
          modTypes := RegExList.Cons(RegEx.Compile(regExStr), modTypes)
        EXCEPT
          RegEx.Error(x) =>
          Debug.Error(F("Cannot compile regex /%s/ : RegEx.Error : %s",
                        regExStr,
                        x))
        END
      END
    END;

    IF pp.keywordPresent("-modify") THEN
      WITH char = pp.getNext(),
           cmd  = pp.getNext() DO
        IF Text.Length(char) # 1 THEN
          Debug.Error("?not a character : " & char)
        END;
        EVAL stdModification.modifiers.put(Text.GetChar(char, 0), cmd)
      END
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

    WHILE pp.keywordPresent("-scminit") OR pp.keywordPresent("-S") DO
      scmFiles.addhi(pp.getNext())
    END;
    
    pp.skipParsed();
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF stdModification.modifiers.size() # 0 THEN
    WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
      FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
        scmarr[i] := scmFiles.get(i)
      END;
      TRY
        SchemeStubs.RegisterStubs();
        scm := NEW(SchemeM3.T).init(ARRAY OF Pathname.T{ "require", "m3" });
        FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
          WITH rd = FileRd.Open(scmarr[i]) DO
            EVAL scm.loadRd(rd, scmarr[i])
          END
        END
      EXCEPT
        Scheme.E(x) =>
        Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
      END
    END
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
