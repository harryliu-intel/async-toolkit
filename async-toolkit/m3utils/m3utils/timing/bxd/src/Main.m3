MODULE Main;
IMPORT CardSeq;
IMPORT ParseParams;
IMPORT Debug;
IMPORT OSError;
IMPORT Rd, FileRd;
IMPORT AL;
IMPORT RefSeq;
IMPORT Pathname;
IMPORT Wr;
IMPORT Text;
IMPORT FileWr;
FROM Fmt IMPORT F;
IMPORT Stdio;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

TYPE
  Strategy = { Space };

CONST
  StrategyNames = ARRAY Strategy OF TEXT { "space" };

TYPE
  ColFinder = PROCEDURE (line : TEXT) : CardSeq.T;

  Output = { Csv };

  GenRec = REF RECORD
    output : Output;
    path   : Pathname.T;
    wr     : Wr.T;
  END;

CONST Finders = ARRAY Strategy OF ColFinder { SpaceFinder };

      
  
VAR
  minColSpaces := 2;
  strategy     := Strategy.Space;

  generators := NEW(RefSeq.T).init();
  
PROCEDURE SpaceFinder(line : TEXT) : CardSeq.T =
  VAR
    seq := NEW(CardSeq.T).init();
    q : CARDINAL;
    spaces := TRUE; (* lately reading spaces *)
    nspaces := 0;
  BEGIN
    q := 0;
    WHILE q < Text.Length(line) DO
      WITH c = Text.GetChar(line, q) DO
        IF NOT spaces THEN
          IF c = ' ' THEN
            INC(nspaces)
          ELSE
            nspaces := 0
          END;
          IF nspaces = minColSpaces THEN
            spaces := TRUE
          END
        ELSE (* doing spaces *)
          IF c # ' ' THEN 
            seq.addhi(q);
            spaces := FALSE
          END
        END
      END;
      INC(q)
    END;
    RETURN seq
  END SpaceFinder;

PROCEDURE Generate(line : TEXT; gen : GenRec; cols : CardSeq.T)
  RAISES { Wr.Failure } =
  VAR
    len := Text.Length(line);
  BEGIN
    FOR i := 0 TO cols.size() - 2 DO
      VAR
        start := cols.get(i);
        stop  := cols.get(i + 1) - 1;
      BEGIN
        WHILE Text.GetChar(line, stop) = ' ' DO DEC(stop) END;
        Wr.PutText(gen.wr, Text.Sub(line, start, stop - start + 1));
        Wr.PutChar(gen.wr, ',')
      END
    END;
    VAR
      start := cols.get(cols.size() - 1);
      stop  := len - 1;
    BEGIN
      WHILE Text.GetChar(line, stop) = ' ' DO DEC(stop) END;
      Wr.PutText(gen.wr, Text.Sub(line, start, stop - start + 1));
      Wr.PutChar(gen.wr, '\n')
    END
  END Generate;
  
VAR
  ifn : Pathname.T := NIL;
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
BEGIN

  TRY
    IF pp.keywordPresent("-strategy") THEN
      (* not yet *)
    END;

    IF pp.keywordPresent("-csv") THEN
      VAR
        rec := NEW(GenRec,
                   output := Output.Csv,
                   path   := pp.getNext());
      BEGIN
        generators.addhi(rec)
      END
    END;

    IF pp.keywordPresent("-f") THEN
      ifn := pp.getNext()
    END;

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF ifn = NIL THEN
    Debug.Error("Must specify filename with -f")
  END;

  VAR
    rd : Rd.T;
  BEGIN
    TRY
      IF TE(ifn, "-") THEN
        rd := Stdio.stdin
      ELSE
        rd := FileRd.Open(ifn)
      END
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Can't open input %s : OSError.E : %s", ifn, AL.Format(x)))
    END;

    FOR i := 0 TO generators.size() - 1 DO
      WITH rec = NARROW(generators.get(i),GenRec) DO
        IF TE(rec.path, "-") THEN
          rec.wr := Stdio.stdout
        ELSE
          TRY
            rec.wr := FileWr.Open(rec.path)
          EXCEPT
            OSError.E(x) =>
            Debug.Error(F("Can't open output %s : OSError.E : %s", rec.path, AL.Format(x)))
          END;
        END
      END 
    END;
    
    VAR
      dummy : TEXT;
      line  : TEXT;
    BEGIN
      TRY
        dummy := Rd.GetLine(rd);
        line  := Rd.GetLine(rd);
        dummy := Rd.GetLine(rd);
        WITH cols = Finders[strategy](line) DO
          LOOP
            line := Rd.GetLine(rd);
            FOR i := 0 TO generators.size() - 1 DO
              TRY
                Generate(line, generators.get(i), cols)
              EXCEPT
                Wr.Failure(x) => 
                Debug.Error(F("I/O error writing to %s : Wr.Failure : %s",
                              NARROW(generators.get(i),GenRec).path,
                              AL.Format(x)))
              END
            END
          END
        END
      EXCEPT
        Rd.EndOfFile => (* skip *)
      |
        Rd.Failure(x) =>
        Debug.Error(F("I/O error reading from %s : Rd.Failure : %s",
                      ifn, AL.Format(x)))
      END;

      FOR i := 0 TO generators.size() - 1 DO
        WITH g = NARROW(generators.get(i), GenRec) DO
          TRY
            Wr.Close(g.wr)
          EXCEPT
            Wr.Failure(x) =>
            Debug.Error(F("I/O error writing to %s [on close]: Wr.Failure : %s",
                          g.path, AL.Format(x)))
          END
        END
      END
    END
  END
END Main.
