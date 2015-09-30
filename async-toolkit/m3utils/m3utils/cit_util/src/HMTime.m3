(* $Id: HMTime.m3,v 1.1 2009/05/05 08:07:02 mika Exp $ *)

MODULE HMTime;
IMPORT TextReader, Scan, Lex, FloatMode, Date, Fmt;
IMPORT FinDate, Word;

PROCEDURE Format(t : T) : TEXT =
  CONST
    I = Fmt.Int;
  BEGIN
    RETURN Fmt.F("%02s:%02s:%02s", I(t.hour), I(t.minute), I(t.second))
  END Format;

PROCEDURE Check(h,m,s : INTEGER) RAISES { ParseError } =
  BEGIN
    (* since we don't have the date here, we permit a leap second at
       the end of any day.  in reality, leap seconds can only occur at 
       the end of the month, and HAVE only occurred on the last days of
       June and December. *)
    IF h < 0 OR h > 23 OR 
       m < 0 OR m > 59 OR 
       s < 0 OR (s > 59 AND (h < 23 OR m < 59)) OR s > 60 THEN
      RAISE ParseError
    END
  END Check;

PROCEDURE Parse(t : TEXT) : T RAISES { ParseError } = 
  BEGIN
    TRY
      VAR
        reader := NEW(TextReader.T).init(t);
        h, m := Scan.Int(reader.nextE(":"));
      BEGIN
        IF NOT reader.empty() THEN
          WITH s = Scan.Int(reader.nextE(":")) DO
            Check(h,m,s);
            RETURN T { h, m, s }
          END
        ELSE
          RETURN T { h, m, 0 }
        END
      END
    EXCEPT
      TextReader.NoMore, Lex.Error, FloatMode.Trap => RAISE ParseError
    END
  END Parse;

PROCEDURE ParseSubsecond(t : TEXT; VAR sub : LONGREAL) : T RAISES { ParseError } = 
  BEGIN
    TRY
      VAR
        reader := NEW(TextReader.T).init(t);
        h, m := Scan.Int(reader.nextE(":"));
      BEGIN
        IF NOT reader.empty() THEN
          WITH s = Scan.Int(reader.nextE(".")) DO
            Check(h,m,s);
            IF NOT reader.empty() THEN 
              WITH subT = "0." & reader.nextE("") DO
                sub := Scan.LongReal(subT)
              END
            ELSE
              sub := 0.0d0
            END;
            RETURN T { h, m, s }
          END
        ELSE
          sub := 0.0d0;
          RETURN T { h, m, 0 }
        END
      END
    EXCEPT
      TextReader.NoMore, Lex.Error, FloatMode.Trap => RAISE ParseError
    END
  END ParseSubsecond;

PROCEDURE Truncate(READONLY d : Date.T) : T =
  BEGIN RETURN T { d.hour, d.minute, d.second } END Truncate;

PROCEDURE Assemble(READONLY t : T;
                   READONLY f : FinDate.T;
                   proto : Date.T) : Date.T =
  BEGIN
    proto.hour   := t.hour;
    proto.minute := t.minute;
    proto.second := t.second;

    proto.year  := f.year;
    proto.month := VAL(f.month-1,Date.Month);
    proto.day   := f.day;

    RETURN proto
  END Assemble;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    IF    a.hour   > b.hour   THEN RETURN  1 
    ELSIF a.hour   < b.hour   THEN RETURN -1
    ELSIF a.minute > b.minute THEN RETURN  1
    ELSIF a.minute < b.minute THEN RETURN -1
    ELSIF a.second > b.second THEN RETURN  1
    ELSIF a.second < b.second THEN RETURN -1
    ELSE
      RETURN 0
    END
  END Compare;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN RETURN SecondInDay(a) END Hash;

PROCEDURE SecondInDay(READONLY a : T) : CARDINAL =
  BEGIN RETURN (a.hour * 60 + a.minute) * 60 + a.second END SecondInDay;

PROCEDURE Advance(READONLY a : T; bySeconds : CARDINAL) : T 
  RAISES { Overflow } =
  BEGIN RETURN FromSeconds(SecondInDay(a)+bySeconds) END Advance;

PROCEDURE FromSeconds(s : CARDINAL) : T RAISES { Overflow } =
  CONST
    SecsInDay = 86400;
  VAR
    t : T;
  BEGIN
    IF s >= SecsInDay THEN RAISE Overflow(s - SecsInDay) END;
    
    t.second :=  s MOD 60;
    t.minute := (s DIV 60) MOD 60;
    t.hour   :=  s DIV 3600;

    RETURN t
  END FromSeconds;

BEGIN END HMTime.
