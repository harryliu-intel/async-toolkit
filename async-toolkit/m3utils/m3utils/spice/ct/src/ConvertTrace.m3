MODULE ConvertTrace EXPORTS Main;

(* ct program -- 

   convert 
          spice trace files (ASCII .tr0 files) 
                    to 
          aspice .names and .trace files

   Aspice output file is created in the fast format (blocked by node)

   Algorithm is external (uses numerous disk files for reordering)

   Author : Mika Nystrom <mika.nystroem@intel.com>

   ct [-rename <dutName>] [-scaletime <timeScaleFactor>] [-offsettime <timeOffset>] [-offsetvoltage <voltageOffset>] [-dosources] <inFileName> <outFileRoot>

   will generate <outFileRoot>.trace and <outFileRoot>.names

*)

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
IMPORT OSError, AL;
IMPORT Thread;
IMPORT Params;
IMPORT UnsafeReader;

<*FATAL Thread.Alerted*>

VAR doDebug := Debug.DebugThis("CT");

CONST Usage = "[-rename <dutName>] [-scaletime <timeScaleFactor>] [-offsettime <timeOffset>] [-offsetvoltage <voltageOffset>] [-dosources] <inFileName> <outFileRoot>";

PROCEDURE StartsWith(READONLY buf, pfx : ARRAY OF CHAR) : BOOLEAN =
  BEGIN 
    RETURN NUMBER(buf) >= NUMBER(pfx) AND SUBARRAY(buf,0,NUMBER(pfx)) = pfx
  END StartsWith;

TYPE CSet = SET OF CHAR; 
CONST iSet = CSet { 'i', 'I' };

PROCEDURE DoNames(READONLY line : ARRAY OF CHAR;
                  <*UNUSED*>f : BOOLEAN) : CARDINAL
  RAISES { SyntaxError } =

  PROCEDURE Push(s, l : CARDINAL; isCurr : BOOLEAN) =
    VAR
      pfx := "";
    BEGIN
      IF isCurr THEN pfx := "I:" END;
      names.addhi(pfx & RenameBack(Text.FromChars(SUBARRAY(line,s,l-s-1))));
      INC(c)
    END Push;

  PROCEDURE Get(s : CSet) RAISES { SyntaxError } =
    BEGIN
      IF NOT line[p] IN s THEN
        RAISE SyntaxError("character " & Int(p) & " '" & Text.FromChar(line[p]) & "' unexpected")
      END;
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

EXCEPTION SyntaxError(TEXT);
          
PROCEDURE DoData(READONLY line : ARRAY OF CHAR; 
                 f             : BOOLEAN) : CARDINAL 
  RAISES { ShortRead, SyntaxError } =

  (* read data from trace file in hspice format

     this routine handles the text format output of hspice 
  *)

  PROCEDURE Char() : CHAR RAISES { ShortRead } =
    BEGIN
      IF p > LAST(line) THEN 
        RAISE ShortRead 
      ELSE
        RETURN line[p]
      END
    END Char;

  PROCEDURE Get(c : CHAR)  RAISES { ShortRead, SyntaxError } =
    BEGIN
      WITH cc = Char() DO
        IF cc # c THEN RAISE SyntaxError("expected '" & Text.FromChar(c) & "' got '" & Text.FromChar(cc) & "'")
        END
      END;
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

  PROCEDURE GetLR(VAR z : LONGREAL) : BOOLEAN
    RAISES { ShortRead, SyntaxError } =
    VAR  
      len : CARDINAL;
      f : INTEGER;
      m := 0;
      x := 0;
      neg : BOOLEAN;
    BEGIN
      TRY
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
    EXCEPT
      SyntaxError(e) => RAISE SyntaxError("GetLR : " & e)
    END
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

        (* process time timestamp *)
        TRY
          EVAL GetLR(lbuff[lbq,lbp]);
        EXCEPT
          SyntaxError(e) => RAISE SyntaxError("Getting timestamp : " & e)
        END;
        lbuff[lbq,lbp] := timeScaleFactor*(lbuff[lbq,lbp]+timeOffset);
        IF doDebug THEN
          Debug.Out(F("time %s", LongReal(lbuff[lbq,lbp])))
        END;
        INC(lbq);
        
        EVAL GetInt(dummy); (* should really assert this is = names.size() *)
      END;
      TRY
      WHILE GetLR(z) DO
        lbuff[lbq,lbp] := z*voltageScaleFactor+voltageOffset;
        INC(lbq); INC(got)
      END;
      EXCEPT
        SyntaxError(e) => RAISE SyntaxError("Getting value : " & e)
      END;
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
              RAISES { ShortRead, SyntaxError };

PROCEDURE NullParser(<*UNUSED*>READONLY line : ARRAY OF CHAR; 
                     <*UNUSED*>f : BOOLEAN) : CARDINAL =
  BEGIN
    RETURN 1
  END NullParser;

CONST 
  MaxMem = 16*1024*1024; (* fit at least one row *)

PROCEDURE FlushData() =
  (* flush data into temp directory *)
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
  VAR
    wr : Wr.T;
    nFn := ofn & ".names";
  BEGIN
    TRY
      wdWr := NEW(REF ARRAY OF Wr.T, names.size());
      
      TRY
        wr := FileWr.Open(nFn)
      EXCEPT
        OSError.E(x) => Debug.Error("Unable to open names file \"" & nFn & "\" : OSError.E : " & AL.Format(x))
      END;
      
      FOR i := 0 TO names.size()-1 DO
        WITH fn = wd & "/" & FormatFN(i) DO
          TRY
            WITH wr2 = FileWr.Open(fn) DO
              wdWr[i] := wr2
            END
          EXCEPT
            OSError.E(x) => Debug.Error("Unable to temp file \"" & fn & "\" : OSError.E : " & AL.Format(x))
          END
        END;
        WITH nm = TextUtils.ReplaceChar(names.get(i), ':', '_') DO
          (* aplot has trouble with colons in node names, so rename those,
             sorry about any clashes ... *)
          Wr.PutText(wr, nm)
        END;
        Wr.PutChar(wr, '\n')
      END;
      Wr.Close(wr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Unable to write names file \"" & nFn & "\" : Wr.Failure : " & AL.Format(x))
    END
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
      Debug.Error("Cant convert node \"" & otxt & "\"");
      <*ASSERT FALSE*>
    END
  END RenameBack;

PROCEDURE WriteTrace() =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  VAR
    buf : ARRAY [0..8191] OF CHAR;
    tFn := ofn & ".trace";
    tWr : Wr.T;
  BEGIN
    TRY
      tWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open trace file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;
    
    UnsafeWriter.WriteI(tWr, 1);
    UnsafeWriter.WriteI(tWr, TRUNC(Time.Now()));
    UnsafeWriter.WriteI(tWr, names.size());

    FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO
      WITH fn = wd & "/" & FormatFN(i) DO
        TRY
          WITH rd = FileRd.Open(fn) DO
            LOOP
              WITH n = Rd.GetSub(rd, buf) DO
                IF n = 0 THEN EXIT END;
                Wr.PutString(tWr, SUBARRAY(buf,0,n))
              END
            END;
            Rd.Close(rd)
          END
        EXCEPT
          OSError.E(x) => Debug.Error("Unable to open temp file \"" & fn & "\" for reading : OSError.E : " & AL.Format(x))
        |
          Rd.Failure(x) => Debug.Error("Read error on temp file \"" & fn & "\" for reading : Rd.Failure : " & AL.Format(x))
        |
          Wr.Failure(x) => Debug.Error("Write error on trace file, lately reading \"" & fn & "\" : Wr.Failure : " & AL.Format(x))
        END
      END
    END;
    TRY
      Wr.Close(tWr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Trouble closing trace file : Wr.Failure : " &
        AL.Format(x))
    END
  END WriteTrace;

PROCEDURE WriteSources() =
  (* read data from each file in temp directory and output
     in reordered trace format for fast aplot access *)
  VAR
    tFn := ofn & ".sources";
    sWr : Wr.T;
    time, data : REF ARRAY OF LONGREAL;
  BEGIN
    TRY
      sWr := FileWr.Open(tFn)
    EXCEPT
      OSError.E(x) => Debug.Error("Unable to open sources file \"" & tFn & "\" for writing : OSError.E : " & AL.Format(x))
    END;

    TRY
      Wr.PutText(sWr, "* sources file generated by ConvertTrace.m3\n");
    EXCEPT
      Wr.Failure(x) => Debug.Error("Write error on sources file : Wr.Failure : " & AL.Format(x))
    END;
    
    FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO
      WITH fn = wd & "/" & FormatFN(i) DO
        TRY
          WITH rd = FileRd.Open(fn) DO

            IF i = FIRST(wdWr^) THEN
              (* time array *)
              Rd.Seek(rd, LAST(CARDINAL));
              WITH len  = Rd.Index(rd),
                   aLen = len DIV 4 (* compute # of timesteps *)
               DO
                Rd.Seek(rd, 0);
                time := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc time *)
                data := NEW(REF ARRAY OF LONGREAL, aLen); (* alloc time *)
                UnsafeReader.ReadLRA(rd, time^)
              END
            ELSE
              Wr.PutText(sWr, "* source for " & names.get(i) & "\n");
              UnsafeReader.ReadLRA(rd, data^);
              Wr.PutText(sWr, F("V%s src%s 0 PWL (\n", Int(i), Int(i)));
              FOR i := FIRST(data^) TO LAST(data^) DO
                Wr.PutText(sWr, F("+   %20s       %20s\n",
                                  LongReal(time[i]),
                                  LongReal(data[i])))
              END;
              Wr.PutText(sWr, "+)\n\n");
            END;

            Rd.Close(rd)
          END
        EXCEPT
          OSError.E(x) => Debug.Error("Unable to open temp file \"" & fn & "\" for reading : OSError.E : " & AL.Format(x))
        |
          Rd.Failure(x) => Debug.Error("Read error on temp file \"" & fn & "\" for reading : Rd.Failure : " & AL.Format(x))
        |
          Wr.Failure(x) => Debug.Error("Write error on sources file, lately reading \"" & fn & "\" : Wr.Failure : " & AL.Format(x))
        END
      END
    END;
    TRY
      Wr.Close(sWr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("Trouble closing sources file : Wr.Failure : " &
        AL.Format(x))
    END
  END WriteSources;

PROCEDURE ParseTr0()
  RAISES { Rd.Failure, ShortRead, SyntaxError } =
  VAR
    buf : ARRAY [0..8191] OF CHAR;
  BEGIN
    (* assumed file structure:

       <stuff to ignore>
       #N <node names, may be multiline>
       #C <timestep data, may be multiline, same # of cols as nodenames>
       #C <repeated>
       #;
       EOF

    *)
    
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
                (* this is a line of node names, parse it with DoNames *)
                parser := DoNames
              ELSIF StartsWith(line,ARRAY OF CHAR { '#', 'C' }) THEN
                (* this is a timestep, parse it with DoData

                   BUT

                   if the last line we parsed was the last line of N,
                   above, then we need to write the .names file for aplot 
                *)
                IF parser = DoNames THEN
                  (* must write out names before we forget! *)
                  WriteNames();

                  WITH n = names.size(),
                       l = BYTESIZE(LONGREAL),
                       q = MaxMem DIV (n * l) DO
                    lbuff := NEW(REF ARRAY OF ARRAY OF LONGREAL, n, q)
                  END;
                END;

                (* now parse the timestep data *)
                parser := DoData
              ELSIF StartsWith(line,ARRAY OF CHAR { '#', ';' }) THEN
                (* this is the last line of the file, skip it *)
                parser := NullParser
              END;
              start := 2
            END;

            EVAL parser(SUBARRAY(line,start,n-start),first)
          END
        END
      END;
      INC(lNo)
    END
  END ParseTr0;

VAR
  names := NEW(TextSeq.T).init();
  ifn, ofn : Pathname.T;

  rd  : Rd.T;

  lbuff : REF ARRAY OF ARRAY OF LONGREAL;
  lbp := 0; lbq := 0;

  Exp : ARRAY [-300..300] OF LONGREAL;
  parser : AParser := NullParser; (* start skipping lines *)
  start : CARDINAL;
  first : BOOLEAN;
  wdWr : REF ARRAY OF Wr.T;
  wd := "ct.work";
  dutName : TEXT := NIL;
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  timeScaleFactor, voltageScaleFactor := 1.0d0;
  timeOffset, voltageOffset := 0.0d0;
  lNo := 1;
  doSources := FALSE;
BEGIN
  TRY
    IF    pp.keywordPresent("-rename") THEN
      dutName := pp.getNext()
    END;
    IF pp.keywordPresent("-scaletime") THEN
      timeScaleFactor := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-scalevoltage") THEN
      voltageScaleFactor := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-offsettime") THEN
      timeOffset := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-offsetvoltage") THEN
      voltageOffset := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-dosources") THEN
      doSources := TRUE
    END;
    
    ifn := pp.getNext();
    ofn := pp.getNext();
    
    pp.finish();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    rd  := FileRd.Open(ifn)
  EXCEPT
    OSError.E(x) => Debug.Error("Trouble opening input file \"" & ifn & "\": OSError.E : " & AL.Format(x))
  END;
  
  names.addhi("TIME");
  TRY FS.CreateDirectory(wd) EXCEPT ELSE END;

  FOR i := FIRST(Exp) TO LAST(Exp) DO
    Exp[i] := Math.pow(10.0d0,FLOAT(i,LONGREAL))
  END;

  TRY
    ParseTr0()
  EXCEPT
    SyntaxError(e) => Debug.Error("Syntax error on line " & Int(lNo) & " : " &
      e)
  |
    ShortRead => Debug.Warning("Short read on final line, data may be corrupted")
  |
    Rd.Failure(x) => Debug.Error("Trouble reading input file : Rd.Failure : " & AL.Format(x))
  END;

  TRY
    FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO Wr.Close(wdWr[i]) END;
  EXCEPT
    Wr.Failure(x) => Debug.Error("Trouble closing temp files : Wr.Failure : " &
      AL.Format(x))
  END;

  WriteTrace();

  IF doSources THEN
    WriteSources()
  END

END ConvertTrace.
