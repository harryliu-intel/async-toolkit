(* $Id: FactorialSuper.m3,v 1.7 2006/03/06 02:29:03 mika Exp $ *)

MODULE FactorialSuper;
IMPORT FactorialValues AS Values;
FROM FactorialBindings IMPORT Type;
IMPORT Wr, Fmt, Thread, Rd;
IMPORT TextReader, Text, Process;
IMPORT FloatMode, Lex, TextSeq, Scan;
IMPORT RefSeq, RefList;
IMPORT TextSet, TextSetDef;

IMPORT FactorialVarBindings;
FROM FactorialVarBindings IMPORT BoolBinding, IntBinding, LRBinding, 
                                 TextBinding;

<* FATAL Thread.Alerted *>


REVEAL
  T = Class BRANDED OBJECT
  METHODS
    extendValues() := ExtendValues;
    parseVI(string : TEXT; mappers : RefSeq.T) : REF ARRAY OF CARDINAL RAISES { Lex.Error, FloatMode.Trap, Error } := ParseVI;
  OVERRIDES
    typeOf := TypeOf;
    writeHeader := WriteHeader;
    writeResults := WriteResults;
    close := Close;
    initFromRd := InitFromRd;
    addBoolVar := AddBoolVar;
    addIntVar := AddIntVar;
    addLRVar := AddLRVar;
    addTextVar := AddTextVar;
    findFactor := FindFactor;
    varyingFactors := VaryingFactors;
  END;

PROCEDURE TypeOf(t : T; index : CARDINAL) : Type =
  BEGIN
    TYPECASE t.values[index] OF
      Values.Int => RETURN Type.Integer
    |
      Values.Bool => RETURN Type.Boolean
    |
      Values.LR => RETURN Type.LongReal
    |
      Values.TX => RETURN Type.Text
    ELSE
      <* ASSERT FALSE *>
    END
  END TypeOf;

CONST IntName = "INTEGER";
CONST BoolName = "BOOLEAN";
CONST LRName = "LONGREAL";
CONST TXName = "TEXT";

PROCEDURE UnQuote(t: TEXT) : TEXT =
  BEGIN
    IF Text.GetChar(t, 0) # '"' OR 
       Text.GetChar(t, Text.Length(t)-1) # '"' THEN
      RETURN t
    END;
    RETURN Text.Sub(t, 1, Text.Length(t)-2)
  END UnQuote;

PROCEDURE WriteHeader(t : T; wr : Wr.T) RAISES { Wr.Failure } =
  BEGIN
    (* write header to wr *)
    Wr.PutText(wr, "FACTORIALDESIGN " & t.name & "\n");

    (* all the variables *)
    FOR i := FIRST(t.values^) TO LAST(t.values^) DO
      Wr.PutText(wr, "VAR " & Fmt.Int(i) & " " & t.values[i].named & " ");
      TYPECASE t.values[i] OF
        Values.Int =>   Wr.PutText(wr, IntName)
      | 
        Values.Bool =>  Wr.PutText(wr, BoolName)
      |
        Values.LR =>    Wr.PutText(wr, LRName)
      |
        Values.TX =>    Wr.PutText(wr, TXName)
      ELSE
        <* ASSERT FALSE *> (* should not have any "plain Values" *)
      END;

      Wr.PutText(wr, " :");

      (* put out all the values *)

      WITH v = t.values[i] DO
        FOR j := 0 TO v.n-1 DO
          Wr.PutChar(wr, ' ');
          Wr.PutText(wr, v.formatV(j))
        END
      END;
      Wr.PutChar(wr, '\n')
    END;

    FOR i := FIRST(t.responses^) TO LAST(t.responses^) DO
      Wr.PutText(wr, "RESPONSE " & Fmt.Int(i) & " " & t.responses[i] & "\n")
    END;
    Wr.Flush(wr)
  END WriteHeader;

PROCEDURE WriteResults(t : T; wr : Wr.T;
                        bindings : B; READONLY data : ARRAY OF LONGREAL) RAISES { Wr.Failure } =
  BEGIN
    <* ASSERT NUMBER(data) = NUMBER(t.responses^) *>

    IF NOT t.wroteHeader THEN t.writeHeader(wr); t.wroteHeader := TRUE END;

    Wr.PutText(wr, "DATA ");
    Wr.PutText(wr, bindings.format());
    FOR i := FIRST(data) TO LAST(data) DO
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, Fmt.LongReal(data[i]))
    END;
    Wr.PutChar(wr, '\n');
    Wr.Flush(wr)
  END WriteResults;

PROCEDURE Close(<*UNUSED*>t : T; wr : Wr.T) RAISES { Wr.Failure } =
  BEGIN
    Wr.PutText(wr, "END\n");
    Wr.Close(wr)
  END Close;

CONST WhiteSpace = " \t";

PROCEDURE ExtendValues(t : T) =
  VAR
    newValues := NEW(REF ARRAY OF Values.T, NUMBER(t.values^) + 1);
  BEGIN
    SUBARRAY(newValues^,0, NUMBER(t.values^)) := t.values^;
    t.values := newValues
  END ExtendValues;

EXCEPTION Error(TEXT);

TYPE
  MapVal = [-1..LAST(CARDINAL)];

PROCEDURE ReMapInt(VAR v : REF ARRAY OF INTEGER;
                   name : TEXT;
                   b : RefList.T) : REF ARRAY OF MapVal =
  VAR
    res := NEW(REF ARRAY OF MapVal, NUMBER(v^));
  BEGIN
    WHILE b # NIL DO
      IF Text.Equal(NARROW(b.head,FactorialVarBindings.T).varName,name) THEN
        IF NOT ISTYPE(b.head,IntBinding) THEN
          Process.Crash("Type mismatch in binding for variable \"" & name &
            "\"!")
        END;
        WITH bb = NARROW(b.head,IntBinding) DO
          FOR i := FIRST(v^) TO LAST(v^) DO
            res[i] := -1;
            FOR j := FIRST(bb.val^) TO LAST(bb.val^) DO
              IF v[i] = bb.val[j] THEN
                res[i] := j
              END
            END
          END;
          v := bb.val
        END;
        RETURN res
      END;
      b := b.tail
    END;
    FOR i := FIRST(v^) TO LAST(v^) DO
      res[i] := i
    END;
    RETURN res
  END ReMapInt;

PROCEDURE ReMapText(v : REF ARRAY OF TEXT;
                   name : TEXT;
                   b : RefList.T) : REF ARRAY OF MapVal =
  VAR
    res := NEW(REF ARRAY OF MapVal, NUMBER(v^));
  BEGIN
    WHILE b # NIL DO
      IF Text.Equal(NARROW(b.head,FactorialVarBindings.T).varName,name) THEN
        IF NOT ISTYPE(b.head,TextBinding) THEN
          Process.Crash("Type mismatch in binding for variable \"" & name &
            "\"!")
        END;
        WITH bb = NARROW(b.head,TextBinding) DO
          FOR i := FIRST(v^) TO LAST(v^) DO
            res[i] := -1;
            FOR j := FIRST(bb.val^) TO LAST(bb.val^) DO
              IF Text.Equal(v[i],bb.val[j]) THEN
                res[i] := j
              END
            END
          END;
          v := bb.val
        END;
        RETURN res
      END;
      b := b.tail
    END;
    FOR i := FIRST(v^) TO LAST(v^) DO
      res[i] := i
    END;
    RETURN res
  END ReMapText;

PROCEDURE ReMapLR(v : REF ARRAY OF LONGREAL;
                   name : TEXT;
                   b : RefList.T) : REF ARRAY OF MapVal =
  VAR
    res := NEW(REF ARRAY OF MapVal, NUMBER(v^));
  BEGIN
    WHILE b # NIL DO
      IF Text.Equal(NARROW(b.head,FactorialVarBindings.T).varName,name) THEN
        IF NOT ISTYPE(b.head,LRBinding) THEN
          Process.Crash("Type mismatch in binding for variable \"" & name &
            "\"!")
        END;
        WITH bb = NARROW(b.head,LRBinding) DO
          FOR i := FIRST(v^) TO LAST(v^) DO
            res[i] := -1;
            FOR j := FIRST(bb.val^) TO LAST(bb.val^) DO
              IF v[i] = bb.val[j] THEN
                res[i] := j
              END
            END
          END;
          v := bb.val
        END;
        RETURN res
      END;
      b := b.tail
    END;
    FOR i := FIRST(v^) TO LAST(v^) DO
      res[i] := i
    END;
    RETURN res
  END ReMapLR;

PROCEDURE ReMapBool(v : REF ARRAY OF BOOLEAN;
                   name : TEXT;
                   b : RefList.T) : REF ARRAY OF MapVal =
  VAR
    res := NEW(REF ARRAY OF MapVal, NUMBER(v^));
  BEGIN
    WHILE b # NIL DO
      IF Text.Equal(NARROW(b.head,FactorialVarBindings.T).varName,name) THEN
        IF NOT ISTYPE(b.head,BoolBinding) THEN
          Process.Crash("Type mismatch in binding for variable \"" & name &
            "\"!")
        END;
        WITH bb = NARROW(b.head,BoolBinding) DO
          FOR i := FIRST(v^) TO LAST(v^) DO
            res[i] := -1;
            FOR j := FIRST(bb.val^) TO LAST(bb.val^) DO
              IF v[i] = bb.val[j] THEN
                res[i] := j
              END
            END
          END;
          v := bb.val
        END;
        RETURN res
      END;
      b := b.tail
    END;
    FOR i := FIRST(v^) TO LAST(v^) DO
      res[i] := i
    END;
    RETURN res
  END ReMapBool;

PROCEDURE InitFromRd(t : T; rd : Rd.T;
                     preBindings : RefList.T) : T RAISES { Rd.Failure } = 
  PROCEDURE CrashMe(mess : TEXT) =
    BEGIN
      Process.Crash("FactorialSuper.InitFromRd: aborting: " & mess & 
        " lately reading line " & Fmt.Int(lNo))
    END CrashMe;

  VAR
    lNo := 0;
    mappers := NEW(RefSeq.T).init();
  BEGIN
    (* parse it ... *)
    t.responses := NEW(REF ARRAY OF TEXT, 0);
    t.values := NEW(REF ARRAY OF Values.T, 0);
    
    TRY
      LOOP
        PROCEDURE NoMoreArgs() RAISES { Error } =
          BEGIN
            IF NOT reader.isEmpty() THEN
              RAISE Error("Too many arguments")
            END
          END NoMoreArgs;

        PROCEDURE Next() : TEXT RAISES { TextReader.NoMore } =
          BEGIN 
            RETURN reader.nextE(WhiteSpace, skipNulls := TRUE);
          END Next;

        PROCEDURE RestOfLine() : TextSeq.T =
          VAR
            res := NEW(TextSeq.T).init();
            word : TEXT;
          BEGIN
            WHILE reader.next(WhiteSpace, word, skipNulls := TRUE) DO
              res.addhi(word)
            END;
            RETURN res
          END RestOfLine;

        VAR
          line := Rd.GetLine(rd);
          reader := NEW(TextReader.T).init(line);
          cmd := reader.nextE(WhiteSpace, skipNulls := TRUE);
        BEGIN
          INC(lNo);
          IF Text.Equal(cmd, "FACTORIALDESIGN") THEN
            t.name := Next()
          ELSIF Text.Equal(cmd, "VAR") THEN
            VAR
              idx := Scan.Int(Next());
              name := Next();
              typeName := Next();
              colon := Next();
              values := RestOfLine();
              n := values.size();
              thisMapper : REF ARRAY OF MapVal;
            BEGIN
              IF NOT Text.Equal(colon,":") THEN
                RAISE Error("Syntax error")
              END;
              
              IF idx # NUMBER(t.values^) THEN
                RAISE Error("values index mismatch in VAR")
              END;
              
              IF    Text.Equal(typeName, IntName) THEN
                VAR
                  v := NEW(REF ARRAY OF INTEGER, n);
                BEGIN
                  FOR i := 0 TO LAST(v^) DO
                    v[i] := Scan.Int(values.get(i))
                  END;

                  thisMapper := ReMapInt(v, name, preBindings);

                  t.addIntVar(name,v^)
                END
              ELSIF Text.Equal(typeName, BoolName) THEN
                VAR
                  v := NEW(REF ARRAY OF BOOLEAN, n);
                BEGIN
                  FOR i := 0 TO LAST(v^) DO
                    v[i] := Scan.Bool(values.get(i))
                  END;

                  thisMapper := ReMapBool(v, name, preBindings);

                  t.addBoolVar(name,v^)
                END
              ELSIF Text.Equal(typeName, LRName) THEN
                VAR
                  v := NEW(REF ARRAY OF LONGREAL, n);
                BEGIN
                  FOR i := 0 TO LAST(v^) DO
                    v[i] := Scan.LongReal(values.get(i))
                  END;

                  thisMapper := ReMapLR(v, name, preBindings);


                  t.addLRVar(name,v^)
                END
              ELSIF Text.Equal(typeName, TXName) THEN
                VAR
                  v := NEW(REF ARRAY OF TEXT, n);
                BEGIN
                  FOR i := 0 TO LAST(v^) DO
                    v[i] := UnQuote(values.get(i))
                  END;

                  thisMapper := ReMapText(v, name, preBindings);

                  t.addTextVar(name,v^)
                END
              ELSE
                RAISE Error("Unknown type \"" & typeName & "\"")
              END;
              mappers.addhi(thisMapper)
            END
          ELSIF Text.Equal(cmd, "RESPONSE") THEN
            VAR
              idx := Scan.Int(Next());
              name := Next();
              newR := NEW(REF ARRAY OF TEXT, NUMBER(t.responses^) +1);
            BEGIN
              SUBARRAY(newR^,0,NUMBER(t.responses^)) := t.responses^;
              newR[idx] := name;
              t.responses := newR
            END
          ELSIF Text.Equal(cmd, "DATA") THEN
            VAR
              name := Next();
              vi := t.parseVI(name,mappers);
              responses := RestOfLine();
              rv := NEW(REF ARRAY OF LONGREAL, responses.size());
            BEGIN
              FOR i := 0 TO responses.size() - 1 DO
                rv[i] := Scan.LongReal(responses.get(i))
              END;
              VAR
                vo := NEW(REF ARRAY OF CARDINAL, NUMBER(vi^));
                skip := FALSE;
                val : MapVal;
              BEGIN
                FOR i := FIRST(vi^) TO LAST(vi^) DO
                  val := NARROW(mappers.get(i),REF ARRAY OF MapVal)[vi[i]];
                  IF val = -1 THEN 
                    skip := TRUE; EXIT 
                  ELSE
                    vo[i] := val
                  END
                END;
                IF NOT skip THEN t.resultCallback(vo^,rv^,name) END
              END
            END
          ELSIF Text.Equal(cmd, "END") THEN
            EXIT
          ELSE
            <* ASSERT FALSE *>
          END;
          NoMoreArgs()
        END
      END
    EXCEPT
      Rd.EndOfFile => CrashMe("short read")
    |
      TextReader.NoMore => CrashMe("too few tokens")
    |
      Error(m) => CrashMe(m)
    |
      Lex.Error, FloatMode.Trap => CrashMe("parse error trying to read number")
    END;
    RETURN t
  END InitFromRd;

PROCEDURE AddBoolVar(t : T; named : TEXT; READONLY values : ARRAY OF BOOLEAN) =
  BEGIN
    t.extendValues();
    VAR
      newValues := NEW(Values.Bool, n := NUMBER(values),
                       named := named, 
                       v := NEW(REF ARRAY OF BOOLEAN, NUMBER(values)));
    BEGIN
      newValues.v^ := values;
      t.values[LAST(t.values^)] := newValues
    END
  END AddBoolVar;

PROCEDURE AddIntVar(t : T; named : TEXT; READONLY values : ARRAY OF INTEGER) =
  BEGIN
    t.extendValues();
    VAR
      newValues := NEW(Values.Int, n := NUMBER(values),
                       named := named, 
                       v := NEW(REF ARRAY OF INTEGER, NUMBER(values)));
    BEGIN
      newValues.v^ := values;
      t.values[LAST(t.values^)] := newValues
    END
  END AddIntVar;

PROCEDURE AddLRVar(t : T; named : TEXT; READONLY values : ARRAY OF LONGREAL) =
  BEGIN
    t.extendValues();
    VAR
      newValues := NEW(Values.LR,  n := NUMBER(values),
                       named := named, 
                       v := NEW(REF ARRAY OF LONGREAL, NUMBER(values)));
    BEGIN
      newValues.v^ := values;
      t.values[LAST(t.values^)] := newValues
    END
  END AddLRVar;

PROCEDURE AddTextVar(t : T; named : TEXT; READONLY values : ARRAY OF TEXT) =
  BEGIN
    t.extendValues();
    VAR
      newValues := NEW(Values.TX,  n := NUMBER(values),
                       named := named, 
                       v := NEW(REF ARRAY OF TEXT, NUMBER(values)));
    BEGIN
      newValues.v^ := values;
      t.values[LAST(t.values^)] := newValues
    END
  END AddTextVar;

PROCEDURE ParseVI(t : T; string : TEXT; mappers : RefSeq.T) : REF ARRAY OF CARDINAL RAISES { Lex.Error, FloatMode.Trap, Error } =
  VAR
    res := NEW(REF ARRAY OF CARDINAL, NUMBER(t.values^));
    vals := NEW(TextReader.T).init(string).shatter("_","");
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := Scan.Int(vals.head);
      IF res[i] > LAST(NARROW(mappers.get(i),REF ARRAY OF MapVal)^) THEN
        RAISE Error("index " & vals.head & " too large for variable #" &
              Fmt.Int(i))
      END;
      vals := vals.tail
    END;
    RETURN res
  END ParseVI;

PROCEDURE FindFactor(t : T; named : TEXT; VAR idx : CARDINAL) : BOOLEAN =
  BEGIN
    FOR i := FIRST(t.values^) TO LAST(t.values^) DO
      IF Text.Equal(t.values[i].named,named) THEN
        idx := i;
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END FindFactor;

PROCEDURE VaryingFactors(t : T; into : TextSet.T) : TextSet.T =
  BEGIN
    IF into = NIL THEN into := NEW(TextSetDef.T).init() END;

    FOR i := FIRST(t.values^) TO LAST(t.values^) DO
      IF t.values[i].n # 1 THEN
        EVAL into.insert(t.values[i].named)
      END
    END;
    RETURN into
  END VaryingFactors;


BEGIN END FactorialSuper.
