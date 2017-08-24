MODULE SpiceObject EXPORTS SpiceObject, SpiceObjectParse;
IMPORT Wr;
IMPORT Rd;
IMPORT Debug;
IMPORT Text;
FROM Fmt IMPORT Int, F;
IMPORT SpiceCircuitList, TextSpiceCircuitTbl;
IMPORT TextSeq;
IMPORT SpiceObjectSeq;
IMPORT SpiceCircuit;
IMPORT SpiceObject, SpiceObjectParse;
IMPORT Scan;
FROM SpiceFileFormat IMPORT White;
IMPORT Word;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    output := Output;
  END;

PROCEDURE Output(t : T; wr : Wr.T; path : TEXT; VAR c : CARDINAL) 
  RAISES { Wr.Failure } =
  BEGIN
    
  END Output;

PROCEDURE ParseLine(VAR circuit : SpiceCircuitList.T; (* circuit stack *)
                    subCkts : TextSpiceCircuitTbl.T;
                    READONLY line : ARRAY OF CHAR;
                    lNo : CARDINAL (* for errors *)) =
  VAR 
    p : CARDINAL := 0;
    str : TEXT;
    ckt : SpiceCircuit.T;
    o : SpiceObject.T := NIL;
  BEGIN
    IF    NUMBER(line) = 0 THEN
      (* skip *)
    ELSIF line[0] = '.' THEN
      (* directive *)
      INC(p);
      IF    HavePrefix(line, p, "SUBCKT") THEN
        (* start subcircuit *)
        WITH new   = NEW(SpiceCircuit.T, 
                         elements := NEW(SpiceObjectSeq.T).init(),
                         params   := NEW(TextSeq.T).init()),
             hadIt = GetWord(line, p, new.name) DO
          <*ASSERT hadIt*>
          WHILE GetWord(line, p, str) DO
            new.params.addhi(str)
          END;
          EVAL subCkts.put(new.name, new);
          circuit := SpiceCircuitList.Cons(new, circuit) (* push *)
        END

      ELSIF HavePrefix(line, p, "ENDS") THEN
        (* end subcircuit *)
        Debug.Out("Closing SUBCKT " & circuit.head.name);
        circuit := circuit.tail (* pop *)
      END
    ELSIF line[0] = '*' THEN
      (* comment *) (* skip for now *)
    ELSIF CaseIns(line[0],'X') THEN
      (* subcell *)
      WITH x     = NEW(SpiceObject.X, terminals := NEW(TextSeq.T).init()),
           gotIt = GetWord(line, p, x.name) DO
        <*ASSERT gotIt*>
        LOOP
          VAR w : TEXT;
              got := GetWord(line, p, w);
          BEGIN
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
                x.type := x.terminals.remhi()
              ELSE (*IF TE(w, "/")*)
                (* easy syntax, typename delimited by slash *)
                WITH gotType = GetWord(line,p,x.type) DO
                  <*ASSERT gotType*> (* syntax *)
                END
              END;

              (* validate typeName *)
              WITH haveIt  = subCkts.get(x.type,ckt) DO
                <*ASSERT haveIt*>  (* definition *)
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
                       data := NEW(TextSeq.T).init());
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
        
        o := m
      END
    ELSIF CaseIns(line[0],'C') THEN
      (* capacitor *)
      VAR nd : TEXT;
          m     := NEW(SpiceObject.C, 
                       terminals := NEW(TextSeq.T).init());
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

        o := m
      END
    ELSIF CaseIns(line[0],'R') THEN
      (* resistor *)
      VAR nd : TEXT;
          m     := NEW(SpiceObject.R, 
                       terminals := NEW(TextSeq.T).init());
          gotNm := GetWord(line,p, m.name);
      BEGIN
        <*ASSERT gotNm*>
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.terminals.addhi(nd);
        (* add res *)
        WITH got = GetWord(line, p, nd) DO <*ASSERT got*> END;
        m.r := ParseValue(nd);
        o := m
      END
    ELSE
      Debug.Warning(F("Unknown element %s in line %s : %s" ,
                      Text.FromChar(line[0]),
                      Int(lNo), Text.FromChars(line)))
    END;

    IF o # NIL THEN circuit.head.elements.addhi(o) END;
    
    IF o # NIL THEN
      o.data := NEW(TextSeq.T).init();
      WHILE GetWord(line, p, str) DO
        o.data.addhi(str)
      END
    END
  END ParseLine;

PROCEDURE CaseIns(a, b : CHAR) : BOOLEAN =
  BEGIN
    IF a >= 'a' AND a <= 'z' THEN
      a := VAL(ORD(a)-ORD('a')+ORD('A'),CHAR)
    END;
    IF b >= 'a' AND b <= 'z' THEN
      b := VAL(ORD(b)-ORD('a')+ORD('A'),CHAR)
    END;
    RETURN a = b
  END CaseIns;

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

PROCEDURE ParseValue(z : TEXT) : LONGREAL =
  BEGIN
    FOR i := FIRST(Suffixes) TO LAST(Suffixes) DO
      IF    RemoveCaseInsSuff(z, Suffixes[i].s) THEN
        RETURN Suffixes[i].mult * Scan.LongReal(z)
      END
    END;
    RETURN Scan.LongReal(z)
  END ParseValue;

PROCEDURE HavePrefix(READONLY line : ARRAY OF CHAR;
                     VAR p : CARDINAL;
                     search : TEXT) : BOOLEAN =
  VAR
    n := Text.Length(search);
  BEGIN
    IF NUMBER(line)-p < n THEN RETURN FALSE END;

    FOR i := 0 TO n-1 DO
      IF NOT CaseIns(Text.GetChar(search,i),line[p+i]) THEN
        RETURN FALSE
      END
    END;
    INC(p,n);
    WHILE p < NUMBER(line) AND line[p] IN White DO
      INC(p)
    END;
    RETURN TRUE
  END HavePrefix;

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

PROCEDURE Hash(a : T) : Word.T = BEGIN
  RETURN Text.Hash(a.name) END Hash;
  
BEGIN END SpiceObject.

