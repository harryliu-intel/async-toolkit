MODULE ConvertTrace EXPORTS Main;
IMPORT FileRd;
IMPORT Rd;
IMPORT Debug;
IMPORT TextSeq;
IMPORT Text;
IMPORT FileWr, Wr;
IMPORT Math;
FROM Fmt IMPORT LongReal,Int, F;
IMPORT FS;
IMPORT UnsafeWriter;
IMPORT Time;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT TextUtils;
IMPORT Scan;
IMPORT Lex, FloatMode;

VAR doDebug := FALSE;

PROCEDURE StartsWith(READONLY buf, pfx : ARRAY OF CHAR) : BOOLEAN =
  BEGIN 
    RETURN NUMBER(buf) >= NUMBER(pfx) AND SUBARRAY(buf,0,NUMBER(pfx)) = pfx
  END StartsWith;

TYPE CSet = SET OF CHAR; 
CONST iSet = CSet { 'i', 'I' };

PROCEDURE DoNames(READONLY line : ARRAY OF CHAR; f : BOOLEAN) : CARDINAL =

  PROCEDURE Push(s, l : CARDINAL; isCurr : BOOLEAN) =
    VAR
      pfx := "";
    BEGIN
      IF isCurr THEN pfx := "I:" END;
      names.addhi(pfx & RenameBack(Text.FromChars(SUBARRAY(line,s,l-s-1))));
      INC(c)
    END Push;

  PROCEDURE Get(s : CSet) =
    BEGIN
      <*ASSERT line[p] IN s *>
      INC(p)
    END Get;

  VAR
    p := 0;
    c := 0;
    n := NUMBER(line);
    s := 0;
    isCurr : BOOLEAN;
  BEGIN
    WHILE p < n DO
      IF line[p] = '\'' THEN
        INC(p);
        isCurr := line[p] IN iSet; 
        Get(CSet { 'v', 'V' } + iSet);
        Get(CSet { '(' } );
        s := p;
        WHILE line[p] # '\'' DO
          INC(p)
        END;
        Push(s,p, isCurr)
      END;
      INC(p)
    END;

    RETURN c
  END DoNames;

EXCEPTION ShortRead;

PROCEDURE DoData(READONLY line : ARRAY OF CHAR; 
                 f             : BOOLEAN) : CARDINAL 
  RAISES { ShortRead } =

  PROCEDURE Char() : CHAR RAISES { ShortRead } =
    BEGIN
      IF p > LAST(line) THEN 
        RAISE ShortRead 
      ELSE
        RETURN line[p]
      END
    END Char;

  PROCEDURE Get(c : CHAR)  RAISES { ShortRead } =
    BEGIN
      <*ASSERT Char() = c *>
      INC(p)
    END Get;

  PROCEDURE GetInt(VAR z : INTEGER) : CARDINAL  RAISES { ShortRead } =
    VAR
      neg := FALSE;
      s := p;
    BEGIN
      WHILE NOT Char() IN SET OF CHAR { '-', '0' .. '9' } DO INC(p) END;
      IF Char() = '-' THEN neg := TRUE; INC(p) END;
      s := p;
      WHILE Char() IN SET OF CHAR { '0' .. '9' } DO 
        z := z * 10 + ORD(Char()) - ORD('0');
        INC(p)
      END;
      IF neg THEN z := -z END;
      RETURN  p-s
    END GetInt;

  PROCEDURE GetLR(VAR z : LONGREAL) : BOOLEAN  RAISES { ShortRead } =
    VAR  
      len : CARDINAL;
      f : INTEGER;
      m := 0;
      x := 0;
      neg : BOOLEAN;
    BEGIN
      WHILE NOT Char() IN SET OF CHAR { '-', '0' .. '9' } DO 
        INC(p); IF p = LAST(line)+1 THEN RETURN FALSE END
      END;
      m := 0;
      EVAL GetInt(m); 
      f := m;
      m := ABS(m);
      neg := m = -f;
      Get('.');
      len := GetInt(m);
      Get('e');
      EVAL GetInt(x);
      z := FLOAT(m,LONGREAL) * Exp[x-len];
      IF neg THEN z := -z END;
      IF doDebug THEN 
        Debug.Out(F("GetLR %s -> %s x (%s - %s) -> %s",
                    Int(f), Int(m), Int(x), Int(len),
                    LongReal(z)))
      END;
      RETURN TRUE
    END GetLR;

  VAR
    p     := 0;
    dummy := 0;
    got := 0;
    z : LONGREAL;
  BEGIN
    TRY
      IF f THEN
        INC(lbp);
        IF lbp = NUMBER(lbuff[0]) THEN
          FlushData();
          lbp := 0
        END;
        lbq := 0;
        EVAL GetLR(lbuff[lbq,lbp]); INC(lbq);
        EVAL GetInt(dummy);
      END;
      WHILE GetLR(z) DO lbuff[lbq,lbp] := z; INC(lbq); INC(got) END;
      RETURN got
    EXCEPT
      ShortRead =>
      IF lbp # 0 THEN  (* abandon current timestep *)
        DEC(lbp);
        lbq := NUMBER(lbuff^);
        FlushData()
      END;
      RAISE ShortRead
    END
  END DoData;

TYPE
  AParser = PROCEDURE(READONLY l : ARRAY OF CHAR; f : BOOLEAN) : CARDINAL 
              RAISES { ShortRead };

PROCEDURE NullParser(<*UNUSED*>READONLY line : ARRAY OF CHAR; 
                     <*UNUSED*>f : BOOLEAN) : CARDINAL =
  BEGIN
    RETURN 1
  END NullParser;

CONST 
  MaxMem = 16*1024*1024; (* fit at least one row *)

PROCEDURE FlushData() =
  BEGIN
    IF doDebug THEN
      Debug.Out(F("FlushData lbp %s lbq %s names %s", Int(lbp), Int(lbq), Int(names.size())));
      <*ASSERT lbq = names.size()*>
      FOR j := 0 TO lbp-1 DO
        FOR i := 0 TO lbq-1 DO
          Debug.Out(LongReal(lbuff[i,j]) & " ")
        END;
        Debug.Out("")
      END
    END;

    FOR i := 0 TO lbq-1 DO
      UnsafeWriter.WriteLRA(wdWr[i], SUBARRAY(lbuff[i],0,lbp))
    END
  END FlushData;

PROCEDURE FormatFN(i : CARDINAL) : TEXT =
  BEGIN RETURN F("%08s", Int(i)) END FormatFN;

PROCEDURE WriteNames() =
  BEGIN
    wdWr := NEW(REF ARRAY OF Wr.T, names.size());

    WITH wr = FileWr.Open(ofn & ".names") DO

      FOR i := 0 TO names.size()-1 DO
        WITH wr2 = FileWr.Open(wd & "/" & FormatFN(i)) DO
          wdWr[i] := wr2
        END;
        Wr.PutText(wr, names.get(i));
        Wr.PutChar(wr, '\n')
      END;
      Wr.Close(wr)
    END;
  END WriteNames;

PROCEDURE RenameBack(txt : TEXT) : TEXT =
  VAR
    otxt := txt;
  BEGIN
    TRY
    IF dutName = NIL THEN 
      RETURN txt
    ELSE
      VAR pfx := dutName & ".";
          p := 0;
          res := "";
      BEGIN
        IF TextUtils.HavePrefix(txt, pfx) THEN
          txt := TextUtils.RemovePrefix(txt, pfx);
        ELSE
          pfx := ""
        END;
        IF  Text.GetChar(txt,p) # 'h'  THEN RAISE Lex.Error END;
        INC(p);
        WHILE p < Text.Length(txt) DO
          res := res & Text.FromChar(VAL(Scan.Int(Text.Sub(txt,p,2),
                                                  defaultBase := 16),CHAR));
          INC(p,2)
        END;
        RETURN pfx & res
      END
    END
    EXCEPT
      Lex.Error, FloatMode.Trap => 
      Debug.Error("Cant convert node \"" & otxt & "\"")
    END
  END RenameBack;

VAR
  names := NEW(TextSeq.T).init();
  ifn, ofn : Pathname.T;

  rd  :Rd.T;

  buf : ARRAY [0..8191] OF CHAR;

  lbuff : REF ARRAY OF ARRAY OF LONGREAL;
  lbp := 0; lbq := 0;

  Exp : ARRAY [-300..300] OF LONGREAL;
  parser : AParser := NullParser;
  start : CARDINAL;
  first : BOOLEAN;
  wdWr : REF ARRAY OF Wr.T;
  wd := "ct.work";
  dutName : TEXT := NIL;
  pp := NEW(ParseParams.T).init(Stdio.stderr);
BEGIN
  IF pp.keywordPresent("-rename") THEN
    dutName := pp.getNext()
  END;

  ifn := pp.getNext();
  ofn := pp.getNext();

  pp.finish();

  rd  := FileRd.Open(ifn);
  
  names.addhi("TIME");
  TRY FS.CreateDirectory(wd) EXCEPT ELSE END;

  FOR i := FIRST(Exp) TO LAST(Exp) DO
    Exp[i] := Math.pow(10.0d0,FLOAT(i,LONGREAL))
  END;

  TRY
  LOOP
    WITH n = Rd.GetSubLine(rd,buf) DO

      IF n = NUMBER(buf) THEN 
        Debug.Error("line too long")
      ELSIF n = 0 THEN
        IF Rd.EOF(rd) THEN
          IF lbp # 0 THEN
            DEC(lbp);
            lbq := NUMBER(lbuff^);
            FlushData()
          END;
          Rd.Close(rd);
          EXIT
        END
      ELSE 
        WITH line = SUBARRAY(buf,0,n) DO
          IF doDebug THEN
            Debug.Out("line " & Text.FromChars(line))
          END;
          start := 0;
          first := FALSE;

          IF    StartsWith(line,ARRAY OF CHAR { '#' }) THEN
            first := TRUE;

            IF    StartsWith(line,ARRAY OF CHAR { '#', 'N' }) THEN
              parser := DoNames
            ELSIF StartsWith(line,ARRAY OF CHAR { '#', 'C' }) THEN
              IF parser = DoNames THEN
                WriteNames();

                WITH n = names.size(),
                     l = BYTESIZE(LONGREAL),
                     q = MaxMem DIV (n * l) DO
                  lbuff := NEW(REF ARRAY OF ARRAY OF LONGREAL, n, q)
                END;
              END;

              parser := DoData
            ELSIF StartsWith(line,ARRAY OF CHAR { '#', ';' }) THEN
              parser := NullParser
            END;
            start := 2
          END;

          EVAL parser(SUBARRAY(line,start,n-start),first)
        END
      END
    END
  END
  EXCEPT
    ShortRead => Debug.Warning("Short read on final line, data may be corrupted")
  END;

  FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO Wr.Close(wdWr[i]) END;

  WITH tWr = FileWr.Open(ofn & ".trace") DO
    UnsafeWriter.WriteI(tWr, 1);
    UnsafeWriter.WriteI(tWr, TRUNC(Time.Now()));
    UnsafeWriter.WriteI(tWr, names.size());

    FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO 
      WITH rd = FileRd.Open(wd & "/" & FormatFN(i)) DO
        LOOP
          WITH n = Rd.GetSub(rd, buf) DO
            IF n = 0 THEN EXIT END;
            Wr.PutString(tWr, SUBARRAY(buf,0,n))
          END
        END;
        Rd.Close(rd)
      END
    END;
    Wr.Close(tWr)
  END

END ConvertTrace.
