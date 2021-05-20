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
IMPORT TextSeq;
IMPORT TextReader;
IMPORT TextList;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

TYPE
  Strategy = { Space, Multi };

CONST
  StrategyNames = ARRAY Strategy OF TEXT { "space", "multi" };

TYPE
  ColFinder = PROCEDURE (line : TEXT) : CardSeq.T;

  Output = { Csv };

  GenRec = REF RECORD
    output : Output;
    path   : Pathname.T;
    wr     : Wr.T;
  END;

CONST Finders = ARRAY Strategy OF ColFinder { SpaceFinder,
                                              SpaceFinder };

      
  
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
            (* add keyword to list of keywords *)
            WITH start = seq.get(seq.size() - 1),
                 lim   = q - nspaces + 1,
                 len   = lim - start,
                 heading = Text.Sub(line, start, len) DO
              headings.addhi(heading)
            END;
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

VAR headings := NEW(TextSeq.T).init();

TYPE Generator = PROCEDURE(line : TEXT; gen : GenRec; cols : CardSeq.T)
  RAISES { Wr.Failure };

CONST Generators = ARRAY Strategy OF Generator { SpaceGen, MultiGen };
      
PROCEDURE SpaceGen(line : TEXT; gen : GenRec; cols : CardSeq.T)
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
  END SpaceGen;

PROCEDURE ToArray(lst : TextList.T; n : CARDINAL) : REF ARRAY OF TEXT =
  VAR
    res := NEW(REF ARRAY OF TEXT, n);
    p := lst;
    j := 0;
  BEGIN
    WHILE p # NIL DO
      res[j] := p.head;

      INC(j); p := p.tail
    END;
    RETURN res
  END ToArray;
    
PROCEDURE MultiGen(line : TEXT; gen : GenRec; cols : CardSeq.T)
  RAISES { Wr.Failure } =
  VAR
    nCols := cols.size();
    reader := NEW(TextReader.T).init(line);
    tokens := reader.shatter(" ", "", TRUE);
    nToks  := TextList.Length(tokens);
    arr    := ToArray(tokens, nToks);
    vals   := NEW(REF ARRAY OF TEXT, nCols);
  BEGIN
    <*ASSERT multiColNum # -1 *>
    <*ASSERT multiColNum < nCols *>
    FOR c := 0 TO nCols - 1 DO
      IF    c > nToks - 1 THEN
        vals[c] := ""
      ELSIF nCols > nToks OR c < multiColNum THEN
        (* if there are not enough tokens to go around we just copy them *)
        vals[c] := arr[c]
      ELSIF c > multiColNum THEN
        (* there are enough tokens that every column has at least one.
           extra tokens go in the special column *)
        vals[c] := arr[c - nCols + nToks]
      ELSE
        <*ASSERT nCols <= nToks AND c = multiColNum*>
        VAR
          str := "";
        BEGIN
          FOR i := c TO c - nCols + nToks DO
            str := str & arr[i];
            IF i # c - nCols + nToks THEN
              str := str & " "
            END
          END;
          vals[c] := str
        END
      END
    END;

    FOR i := 0 TO nCols - 1 DO
      Wr.PutText(gen.wr, vals[i]);
      IF i # nCols - 1 THEN
        Wr.PutChar(gen.wr, ',')
      ELSE
        Wr.PutChar(gen.wr, '\n')
      END
    END
        
  END MultiGen;
  
VAR
  ifn : Pathname.T := NIL;
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
  multiColName : TEXT := NIL;
  multiColNum : [-1..LAST(CARDINAL) ] := -1;
BEGIN

  TRY
    IF pp.keywordPresent("-csv") THEN
      VAR
        rec := NEW(GenRec,
                   output := Output.Csv,
                   path   := pp.getNext());
      BEGIN
        generators.addhi(rec)
      END
    END;

    IF pp.keywordPresent("-multi") THEN
      strategy := Strategy.Multi;
      multiColName := pp.getNext()
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
          IF strategy = Strategy.Multi THEN
            FOR i := 0 TO cols.size() - 1 DO
              IF TE(headings.get(i), multiColName) THEN
                multiColNum := i;
                EXIT
              END
            END;
            IF multiColNum = -1 THEN
              Debug.Error(F("Can't find column named \"%s\"", multiColName))
            END
          END;
          LOOP
            line := Rd.GetLine(rd);
            FOR i := 0 TO generators.size() - 1 DO
              TRY
                Generators[strategy](line, generators.get(i), cols)
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
