MODULE SimMeasurement;
IMPORT Rd;
FROM Fmt IMPORT F, LongReal;
IMPORT Scan;
IMPORT Text;
IMPORT CharSeq;
IMPORT Lex, FloatMode;
IMPORT Thread;
IMPORT Debug;
IMPORT NodeMeasurement, NodeMeasurementSet;
<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED OBJECT OVERRIDES
    visitNodeMeasurements := VisitNodeMeasurementsNOP;
  END;
  
  Default = PublicDef BRANDED OBJECT
  OVERRIDES
    format := FormatDefault;
    visitNodeMeasurements := VisitNodeMeasurementsDefault;
  END;

PROCEDURE VisitNodeMeasurementsNOP(<*UNUSED*>t : T;
                                   <*UNUSED*>set : NodeMeasurementSet.T) =
  BEGIN END VisitNodeMeasurementsNOP;

PROCEDURE FormatDefault(t : Default;
                        dutName : TEXT;
                        cc : ClockConverter) : TEXT =
  BEGIN
    RETURN
      F("MEASURE %s %s %s (%s) (%s)",
        TypeNames[t.type],
        dutName & t.nodeNm,
        ProbeTypeNames[t.quantity],
        FormatTrigger(t.from, dutName, cc),
        FormatTrigger(t.to, dutName, cc))
  END FormatDefault;

PROCEDURE VisitNodeMeasurementsDefault(def : Default; set : NodeMeasurementSet.T) =
  BEGIN
    EVAL set.insert(NodeMeasurement.T { def.nodeNm, def.quantity });
    IF def.from # NIL THEN
      def.from.spec.visitNodeMeasurements(set)
    END;
    IF def.to # NIL THEN
      def.to.spec.visitNodeMeasurements(set)
    END
  END VisitNodeMeasurementsDefault;

REVEAL
  Clock = T BRANDED OBJECT METHODS OVERRIDES format := FormatClock END;
  Time  = T BRANDED OBJECT METHODS OVERRIDES format := FormatTime END;

PROCEDURE FormatClock(<*UNUSED*>t : Clock;
                      <*UNUSED*>dutName : TEXT;
                      <*UNUSED*>cc : ClockConverter) : TEXT =
  BEGIN RETURN ClockName END FormatClock;

PROCEDURE FormatTime(<*UNUSED*>t : Time;
                     <*UNUSED*>dutName : TEXT;
                     <*UNUSED*>cc : ClockConverter) : TEXT =
  BEGIN RETURN TimeName END FormatTime;

CONST ClockName = "%CYCLCE%";
      TimeName  = "%TIME%";

PROCEDURE FormatTrigger(trigger : Trigger;
                        dutName : TEXT;
                        cc : ClockConverter) : TEXT =
  VAR
    new : Trigger;
  BEGIN
    IF trigger = NIL THEN RETURN "" END;

    new := NEW(Trigger, op := trigger.op);

    IF ISTYPE(trigger.spec,Clock) THEN
      new.spec := NEW(Time);
      new.val := cc.timeOfCycle(trigger.val)
    ELSE
      new.spec := trigger.spec;
      new.val := trigger.val
    END;

    RETURN F("%s %s %s",
             LongReal(new.val),
             OpNames[new.op],
             new.spec.format(dutName, cc))
  END FormatTrigger;

PROCEDURE Parse(rd : Rd.T) : T
  RAISES { Rd.Failure, Rd.EndOfFile, ParseError } =
  BEGIN
    WITH token = Token(rd) DO
      IF TE(token, ClockName) THEN
        RETURN NEW(Clock)
      ELSIF TE(token, TimeName) THEN
        RETURN NEW(Time)
      ELSIF NOT TE(token, "MEASURE") THEN
        RAISE ParseError(F("Unknown SimMeasurement \"%s\"",token))
      END
    END;
    VAR
      def := NEW(Default);
    BEGIN
      def.type := ParseType(Token(rd));
      def.nodeNm := Token(rd);
      def.quantity := ParseProbeType(Token(rd));
      Get(rd,"(");
      def.from := ParseTrigger(NEW(Trigger), rd);
      Get(rd,")");
      Get(rd,"(");
      def.to   := ParseTrigger(NEW(Trigger), rd);
      Get(rd,")");
      RETURN def
    END
  END Parse;

PROCEDURE ParseTrigger(trigger : Trigger; rd : Rd.T) : Trigger
  RAISES { ParseError, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    TRY
      WITH tok = Token(rd) DO
        IF TE(tok, ")") THEN
          Rd.UnGetChar(rd);
          RETURN NIL
        ELSE
          trigger.val := Scan.LongReal(tok)
        END
      END
    EXCEPT
      Lex.Error, FloatMode.Trap => RAISE ParseError("expected number")
    END;
    trigger.op  := ParseOp(Token(rd));
    trigger.spec := Parse(rd);
    RETURN trigger
  END ParseTrigger;

CONST White = SET OF CHAR { ' ', '\t' };
CONST Parens = SET OF CHAR { '(', ')' };

PROCEDURE Get(rd : Rd.T; tok : TEXT)
  RAISES { ParseError, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    MustBe(Token(rd),tok)
  END Get;

PROCEDURE Token(rd : Rd.T) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    seq := NEW(CharSeq.T).init();
    c : CHAR;
  BEGIN
    WHILE Rd.GetChar(rd) IN White DO END;
    Rd.UnGetChar(rd);

    c := Rd.GetChar(rd);
    IF c IN Parens THEN
      seq.addhi(c);
    ELSE
      Rd.UnGetChar(rd);
      LOOP
        c := Rd.GetChar(rd);
        IF c IN White THEN EXIT END;
        IF c IN Parens THEN Rd.UnGetChar(rd); EXIT END;
        seq.addhi(c)
      END
    END;

    VAR
      chars := NEW(REF ARRAY OF CHAR, seq.size());
    BEGIN
      FOR i := FIRST(chars^) TO LAST(chars^) DO
        chars[i] := seq.get(i)
      END;
      WITH res = Text.FromChars(chars^) DO
        Debug.Out("SimMeasurement.Token " & res);
        RETURN res
      END
    END
  END Token;

PROCEDURE MustBe(a, b : TEXT) RAISES { ParseError } =
  BEGIN
    IF NOT TE(a,b) THEN
      RAISE ParseError(F("tokens must be equal \"%s\" \"%s\"", a, b))
    END
  END MustBe;
  
PROCEDURE ParseType(txt : TEXT) : Type  RAISES { ParseError } =
  BEGIN
    FOR i := FIRST(TypeNames) TO LAST(TypeNames) DO
      IF TE(TypeNames[i], txt) THEN RETURN i END
    END;
    RAISE ParseError(F("Not a Type \"%s\"", txt))
  END ParseType;

PROCEDURE ParseProbeType(txt : TEXT) : ProbeType  RAISES { ParseError } =
  BEGIN
    FOR i := FIRST(ProbeTypeNames) TO LAST(ProbeTypeNames) DO
      IF TE(ProbeTypeNames[i], txt) THEN RETURN i END
    END;
    RAISE ParseError(F("Not a ProbeType \"%s\"", txt))
  END ParseProbeType;

PROCEDURE ParseOp(txt : TEXT) : Op RAISES { ParseError } =
  BEGIN
    FOR i := FIRST(OpNames) TO LAST(OpNames) DO
      IF TE(OpNames[i], txt) THEN RETURN i END
    END;
    RAISE ParseError(F("Not an Op \"%s\"", txt))
  END ParseOp;
  
BEGIN END SimMeasurement.
  
