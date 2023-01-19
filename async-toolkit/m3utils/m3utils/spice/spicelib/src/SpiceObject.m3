MODULE SpiceObject EXPORTS SpiceObject, SpiceObjectParse;
IMPORT Debug;
IMPORT Text;
FROM Fmt IMPORT Int, F, Bool, LongReal;
IMPORT SpiceCircuitList, TextSpiceCircuitTbl;
IMPORT TextSeq;
IMPORT SpiceObjectSeq;
IMPORT SpiceCircuit;
IMPORT SpiceObject;
IMPORT Scan;
FROM SpiceFileFormat IMPORT White;
IMPORT Word;
IMPORT SpiceError;
FROM Debug IMPORT UnNil;
FROM SpiceParse IMPORT HavePrefix, CaseIns;
IMPORT FloatMode, Lex;
IMPORT TextTextTbl;

CONST TE = Text.Equal;
      LR = LongReal;
      
VAR doDebug := Debug.DebugThis("SpiceObject");
    
REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
  (*  output := Output;*)
  END;
  
(*
  PROCEDURE Output(t : T; wr : Wr.T; path : TEXT; VAR c : CARDINAL) 
  RAISES { Wr.Failure } =
  BEGIN
    <
  END Output;
*)

PROCEDURE IsParamAssign(txt : TEXT) : BOOLEAN =
  (* returns whether it matches the pattern <a>=<b> *)
  BEGIN
    RETURN Text.FindChar(txt, '=') # -1
  END IsParamAssign;

PROCEDURE FmtTextSeq(seq : TextSeq.T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      res := res & " " & seq.get(i)
    END;
    RETURN res
  END FmtTextSeq;
  
PROCEDURE ParseLine(VAR circuit   : SpiceCircuitList.T; (* circuit stack *)
                    subCkts       : TextSpiceCircuitTbl.T;
                    subCktNames   : TextSeq.T;
                    READONLY line : ARRAY OF CHAR;
                    VAR warning   : TEXT)
  RAISES { SpiceError.E } =
  VAR 
    p   : CARDINAL := 0;
    str : TEXT;
    ckt : SpiceCircuit.T;
    o   : SpiceObject.T := NIL;
  BEGIN
    warning := NIL;
    IF    NUMBER(line) = 0 THEN
      (* skip *)
    ELSIF line[p] = '.' THEN
      IF doDebug THEN
        Debug.Out("SpiceObject.ParseLine Got directive")
      END;
      (* directive *)
      IF    HavePrefix(line, p, ".SUBCKT") THEN
        (* start subcircuit *)
        WITH new   = NEW(SpiceCircuit.T, 
                         elements := NEW(SpiceObjectSeq.T).init(),
                         params   := NEW(TextSeq.T).init(),
                         probes   := NEW(TextSeq.T).init()),
             hadIt = GetWord(line, p, new.name) DO
          <*ASSERT hadIt*>
          WHILE GetWord(line, p, str) DO
            <*ASSERT str # NIL*>
            WITH eqPos = Text.FindChar(str, '=') DO
              IF eqPos = -1 THEN
                (* it's a formal parameter name *)
                new.params.addhi(str)
              ELSE
                (* it's actually a value binding *)
                IF new.bindings = NIL THEN
                  new.bindings := NEW(TextTextTbl.Default).init();
                  WITH k = Text.Sub(str,         0, eqPos),
                       v = Text.Sub(str, eqPos + 1, LAST(CARDINAL)) DO
                    EVAL new.bindings.put(k, v)
                  END
                END
              END
            END
          END;
          EVAL subCkts.put(new.name, new);
          subCktNames.addhi(new.name);
          circuit := SpiceCircuitList.Cons(new, circuit) (* push *)
        END

      ELSIF HavePrefix(line, p, ".ENDS") THEN
        (* end subcircuit *)
        IF doDebug THEN
          Debug.Out("Closing SUBCKT " & circuit.head.name)
        END;
        circuit := circuit.tail (* pop *)
      ELSIF HavePrefix(line, p, ".PROBE") THEN
        IF GetWord(line, p, str) THEN
          circuit.head.probes.addhi(str)
        ELSE
          RAISE SpiceError.E(SpiceError.Data {
        msg := "Empty probe : \"" & Text.FromChars(line) & "\""
        })
        END
      ELSE
        RAISE SpiceError.E(SpiceError.Data {
        msg := "Unknown directive in \"" & Text.FromChars(line) & "\""
        })
      END
    ELSIF line[0] = '*' THEN
      (* comment *) (* skip for now *)
    ELSIF CaseIns(line[0],'X') THEN
      (* subcell *)
      WITH x     = NEW(SpiceObject.X,
                       terminals := NEW(TextSeq.T).init(),
                       data      := NEW(TextSeq.T).init()),
           gotIt = GetWord(line, p, x.name) DO
        <*ASSERT gotIt*>
        LOOP
          VAR
            w : TEXT;
            got := GetWord(line, p, w);
          BEGIN
            IF doDebug THEN
              Debug.Out(F("got=%s word %s ", Bool(got), UnNil(w)))
            END;
            (* we have just read a word from the line 
               
               some systems produce

               Xnm tnm0 tnm1 .. tnmN-1 / typeName

               others produce

               Xnm tnm0 tnm1 .. tnmN-1 typeName

               for the former we look for the slash
               for the latter we have to back up once having parsed
               the typeName as a terminal name 
            *)
            IF NOT got OR TE(w, "/") THEN
              IF NOT got THEN
                WHILE IsParamAssign(x.terminals.get(x.terminals.size() - 1)) DO
                  x.data.addhi(x.terminals.remhi())
                END;
                x.type := x.terminals.remhi()
              ELSE (*IF TE(w, "/")*)
                (* easy syntax, typename delimited by slash *)
                WITH gotType = GetWord(line, p, x.type) DO
                  <*ASSERT gotType*> (* syntax *)
                END
              END;

              (* validate typeName *)
              WITH haveIt  = subCkts.get(x.type, ckt) DO
                IF NOT haveIt THEN
                  RAISE SpiceError.E(SpiceError.Data {
                  msg := "Can't find subcircuit of type " & x.type
                  })
                END;

                <*ASSERT haveIt*>  (* definition *)

                IF NOT x.terminals.size() = ckt.params.size()  THEN
                  RAISE SpiceError.E (
                            SpiceError.Data {
                  msg := F("Wrong terminal count: subcircuit type %s \nparams at call site %3s : %s\nterminals %3s           : %s\n",
                           x.type,
                           Int(x.terminals.size()),
                           FmtTextSeq(x.terminals),
                           Int(ckt.params.size()),
                           FmtTextSeq(ckt.params)) })
                END;

                <*ASSERT x.terminals.size() = ckt.params.size()*>
                <*ASSERT x.type # NIL*>
              END;
              EXIT
            END;
            x.terminals.addhi(w)
          END
        END(*POOL*);
        o := x
      END
    ELSIF CaseIns(line[0],'M') THEN
      (* transistor *)
      VAR nd : TEXT;
          m     := NEW(SpiceObject.M, 
                       terminals := NEW(TextSeq.T).init(),
                       data      := NEW(TextSeq.T).init());
          gotNm := GetWord(line,p, m.name);
      BEGIN
        <*ASSERT gotNm*>
        WITH gotDr = GetWord(line, p, nd) DO <*ASSERT gotDr*> END;
        m.terminals.addhi(nd);
        WITH gotGt = GetWord(line, p, nd) DO <*ASSERT gotGt*> END;
        m.terminals.addhi(nd);
        WITH gotSr = GetWord(line, p, nd) DO <*ASSERT gotSr*> END;
        m.terminals.addhi(nd);
        WITH gotBk = GetWord(line, p, nd) DO <*ASSERT gotBk*> END;
        m.terminals.addhi(nd);

        WITH gotType = GetWord(line, p, nd) DO <*ASSERT gotType*> END;
        m.type := nd;

        StuffData(m, line, p);
        
        o := m
      END
    ELSIF CaseIns(line[0],'D') THEN
      (* diode *)
      VAR nd : TEXT;
          d     := NEW(SpiceObject.D, 
                       terminals := NEW(TextSeq.T).init(),
                       data      := NEW(TextSeq.T).init());
          gotNm := GetWord(line,p, d.name);
      BEGIN
        <*ASSERT gotNm*>
        WITH gotAn = GetWord(line, p, nd) DO <*ASSERT gotAn*> END;
        d.terminals.addhi(nd);
        WITH gotCa = GetWord(line, p, nd) DO <*ASSERT gotCa*> END;
        d.terminals.addhi(nd);

        WITH gotModel = GetWord(line, p, nd) DO <*ASSERT gotModel*> END;
        d.type := nd;

        StuffData(d, line, p);

        o := d
      END
    ELSIF CaseIns(line[0],'C') THEN
      (* capacitor *)
      VAR nd : TEXT;
          m     := NEW(SpiceObject.C, 
                       terminals := NEW(TextSeq.T).init(),
                       data      := NEW(TextSeq.T).init());
          gotNm := GetWord(line,p, m.name);
      BEGIN
        <*ASSERT gotNm*>
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        (* add cap *)
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.c := ParseValue(nd);

        StuffData(m, line, p);

        o := m
      END
    ELSIF CaseIns(line[0],'L') THEN
      (* inductor *)
      VAR nd : TEXT;
          m     := NEW(SpiceObject.L, 
                       terminals := NEW(TextSeq.T).init(),
                       data      := NEW(TextSeq.T).init());
          gotNm := GetWord(line,p, m.name);
      BEGIN
        <*ASSERT gotNm*>
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        (* add cap *)
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.l := ParseValue(nd);

        StuffData(m, line, p);

        o := m
      END
    ELSIF CaseIns(line[0],'R') THEN
      (* resistor *)
      VAR nd : TEXT;
          m     := NEW(SpiceObject.R, 
                       terminals := NEW(TextSeq.T).init(),
                       data      := NEW(TextSeq.T).init());
          gotNm := GetWord(line,p, m.name);
      BEGIN
        <*ASSERT gotNm*>
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        (* add res *)
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        TRY
          m.r := ParseValue(nd);
        EXCEPT
          SpiceError.E =>
          Debug.Warning(
              F("Cannot parse resistance \"%s\", substituting 1 ohm.", nd));
          m.r := NEW(RealLiteral, v := 1.0d0)
        END;

        StuffData(m, line, p);

        o := m
      END
    ELSE
      warning := F("unknown element %s : %s" ,
                      Text.FromChar(line[0]),
                      Text.FromChars(line))
    END;

    IF o # NIL AND GetWord(line, p, str) THEN
      RAISE SpiceError.E ( SpiceError.Data { msg := "unexpected " & str })
    END;

    IF o # NIL THEN
      IF o.data = NIL THEN
        o.data := NEW(TextSeq.T).init() (* probably cant happen *)
      END;
      WHILE GetWord(line, p, str) DO
        o.data.addhi(str)
      END
    END;

    IF o # NIL THEN circuit.head.elements.addhi(o) END;
    
  END ParseLine;

TYPE
  Suffix = RECORD
    s : TEXT; mult : LONGREAL
  END;

CONST 
  Suffixes = ARRAY OF Suffix {
  Suffix { "T", 1.0d12 },
  Suffix { "G", 1.0d9 },
  Suffix { "MEG", 1.0d6 }, Suffix { "X", 1.0d6 },
  Suffix { "K", 1.0d3 },
  Suffix { "MIL", 25.4d-6 },
  Suffix { "M", 1.0d-3 },
  Suffix { "U", 1.0d-6 },
  Suffix { "N", 1.0d-9 },
  Suffix { "P", 1.0d-12 },
  Suffix { "F", 1.0d-15 },
  Suffix { "A", 1.0d-18 }
  };


PROCEDURE RemoveCaseInsSuff(VAR z : TEXT; s : TEXT) : BOOLEAN =
  VAR
    n := Text.Length(z);
    nn := Text.Length(s);
  BEGIN
    IF nn > n THEN 
      RETURN FALSE 
    ELSE
      FOR p := 0 TO nn-1 DO
        WITH c = Text.GetChar(z, n-nn+p),
             d = Text.GetChar(s, p) DO
          IF NOT CaseIns(c,d) THEN RETURN FALSE END
        END
      END;
      z := Text.Sub(z, 0, n-nn);
      RETURN TRUE
    END
  END RemoveCaseInsSuff;

PROCEDURE ParseValue(z : TEXT) : RealValue =
  BEGIN
    TRY
      FOR i := FIRST(Suffixes) TO LAST(Suffixes) DO
        IF    RemoveCaseInsSuff(z, Suffixes[i].s) THEN
          RETURN NEW(RealLiteral, v := Suffixes[i].mult * Scan.LongReal(z))
        END
      END;
      RETURN NEW(RealLiteral, v := Scan.LongReal(z))

    EXCEPT
      Lex.Error, FloatMode.Trap =>
      RETURN NEW(RealExpression, x := z)
    END
  END ParseValue;

PROCEDURE GetWord(READONLY line : ARRAY OF CHAR;
                  VAR      p    : CARDINAL;
                  VAR      w    : TEXT           ) : BOOLEAN =
  VAR
    s : CARDINAL;
  BEGIN
    WHILE p < NUMBER(line) AND line[p] IN White DO
      INC(p)
    END;
    IF p = NUMBER(line) THEN RETURN FALSE END;
    s := p;
    WHILE p < NUMBER(line) AND NOT line[p] IN White DO INC(p) END;
    w := Text.FromChars(SUBARRAY(line,s,p-s));
    RETURN TRUE
  END GetWord;

PROCEDURE StuffData(o             : SpiceObject.T;
                    READONLY line : ARRAY OF CHAR;
                    VAR      p    : CARDINAL;
                    permissive    := TRUE)
  RAISES { SpiceError.E } =
  VAR
    nd : TEXT;
  BEGIN
    WHILE GetWord(line, p, nd) DO
      IF permissive OR IsParamAssign(nd) THEN
            o.data.addhi(nd)
      ELSE
        RAISE SpiceError.E (
                  SpiceError.Data { msg := "bad syntax " & nd }
        )
      END
    END
  END StuffData;

PROCEDURE Hash(a : T) : Word.T = BEGIN
  RETURN Text.Hash(a.name) END Hash;

PROCEDURE Format(a : T) : TEXT =
  BEGIN
    TYPECASE a OF
      R(r) => RETURN F("R(%s)", FmtReal(r.r))
    |
      C(c) => RETURN F("C(%s)", FmtReal(c.c))
    |
      M(m) => RETURN F("M(%s)", m.type)
    |
      X(x) => RETURN F("X(%s)", x.type)
    ELSE
      RETURN "UNKNOWN"
    END
  END Format;

PROCEDURE FmtReal(rv : RealValue) : TEXT =
  BEGIN
    TYPECASE rv OF
      RealLiteral(rl) => RETURN LR(rl.v)
    |
      RealExpression(rx) => RETURN rx.x
    ELSE
      <*ASSERT FALSE*>
    END
  END FmtReal;

BEGIN END SpiceObject.

