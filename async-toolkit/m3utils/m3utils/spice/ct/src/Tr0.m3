MODULE Tr0;
IMPORT Rd;
IMPORT Debug;
IMPORT Text;
IMPORT Wr;
IMPORT AL, TextUtils;
FROM Fmt IMPORT LongReal, Int, F;
IMPORT UnsafeWriter;
IMPORT Thread;
IMPORT TextSeq;
IMPORT Scan, FloatMode, Lex;
IMPORT Pathname;
IMPORT Math;
IMPORT TextSet;
IMPORT RegExList;
IMPORT CardSeq;

IMPORT NameControl;
FROM NameControl IMPORT MakeIdxMap, SanitizeNames,
                        WriteNames;

<*FATAL Thread.Alerted*>

TYPE CSet = SET OF CHAR; 
CONST iSet = CSet { 'i', 'I' };


VAR doDebug := Debug.DebugThis("Tr0");

PROCEDURE RenameBack(dutName, txt : TEXT) : TEXT =
  (* what the heck does this do, exactly? *)
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

PROCEDURE FileIndex(nFiles, nNodes, nodeIndex : CARDINAL) : CARDINAL =
  BEGIN
    IF nodeIndex = 0 THEN
      RETURN 0 (* TIME node on its own *)
    ELSE
      WITH nonTimeFiles = nFiles - 1,
           nonTimeIndex = nodeIndex - 1,
           nodesPerFile = (nNodes DIV nonTimeFiles) + 1 DO
        RETURN nonTimeIndex DIV nodesPerFile + 1
      END
    END
  END FileIndex;

PROCEDURE StartsWith(READONLY buf, pfx : ARRAY OF CHAR) : BOOLEAN =
  BEGIN 
    RETURN NUMBER(buf) >= NUMBER(pfx) AND SUBARRAY(buf,0,NUMBER(pfx)) = pfx
  END StartsWith;

PROCEDURE FlushData(READONLY wdWr  : ARRAY OF Wr.T;
                    lbp, lbq       : CARDINAL;
                    names          : TextSeq.T;
                    READONLY lbuff : ARRAY OF ARRAY OF LONGREAL) =

  (* flush data into temp directory *)
  VAR
    nFiles := NUMBER(wdWr);
   
  BEGIN
    
    IF doDebug THEN
      Debug.Out(F("FlushData lbp %s lbq %s names %s", Int(lbp), Int(lbq), Int(names.size())));
      FOR j := 0 TO lbp - 1 DO
        FOR i := 0 TO lbq - 1 DO
          Debug.Out(LongReal(lbuff[i,j]) & " ", 10000)
        END;
        Debug.Out("")
      END
    END;

    (* TIME file has different format *)
    TRY
      UnsafeWriter.WriteLRA(wdWr[FileIndex(nFiles, lbq, 0)],
                            SUBARRAY(lbuff[0], 0, lbp))
    EXCEPT
      Wr.Failure(x) => Debug.Error("Trouble flushing TIME data : Wr.Failure : " &
      AL.Format(x))
    END;
    
    FOR i := 1 TO lbq - 1 DO
      (* format of data file:
         <node index>
         <# of samples>
         <binary sample data>
      *)
      TRY
        WITH wr = wdWr[FileIndex(nFiles, lbq, i)] DO
          UnsafeWriter.WriteI  (wr, i);     
          UnsafeWriter.WriteI  (wr, lbp);
          UnsafeWriter.WriteLRA(wr, SUBARRAY(lbuff[i], 0, lbp))
        END
      EXCEPT
        Wr.Failure(x) => Debug.Error(F("Trouble flushing data for node %s (%s): Wr.Failure : %s", names.get(i), Int(i), 
                                       AL.Format(x)))
      END
    END

  END FlushData;

PROCEDURE NullParser(<*UNUSED*>READONLY line : ARRAY OF CHAR; 
                     <*UNUSED*>f : BOOLEAN) : CARDINAL =
  BEGIN
    RETURN 1
  END NullParser;

(**********************************************************************)

PROCEDURE Parse(wd, ofn        : Pathname.T;
                names          : TextSeq.T;
                maxFiles       : CARDINAL;
                VAR nFiles     : CARDINAL;
                MaxMem         : CARDINAL;

                timeScaleFactor,
                timeOffset,
                voltageScaleFactor,
                voltageOffset  : LONGREAL;

                dutName        : TEXT;
                 
                rd             : Rd.T;
                wait           : BOOLEAN;
                restrictNodes  : TextSet.T;
                restrictRegEx  : RegExList.T)
  RAISES { Rd.Failure, ShortRead, SyntaxError } =

  VAR lbp   : CARDINAL := 0;
      lbq   : CARDINAL;
      rbq   : CARDINAL;
      
  VAR lNo := 1;
      
  VAR lbuff : REF ARRAY OF ARRAY OF LONGREAL;

  PROCEDURE DoNames(READONLY line : ARRAY OF CHAR;
                    <*UNUSED*>f   : BOOLEAN) : CARDINAL
    RAISES { SyntaxError } =

    PROCEDURE Push(s, l : CARDINAL; isCurr : BOOLEAN) =
      VAR
        pfx := "";
      BEGIN
        IF isCurr THEN pfx := "I:" END;
        names.addhi(pfx & RenameBack(dutName,
                                     Text.FromChars(SUBARRAY(line,s,l-s-1))));
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
          IF p = n THEN
            RAISE SyntaxError("file ends in a backslash")
          END;
          isCurr := line[p] IN iSet; 
          Get(CSet { 'v', 'V' } + iSet);
          Get(CSet { '(' } );
          s := p;
          WHILE line[p] # '\'' DO
            INC(p);
            IF p = n THEN
              RAISE SyntaxError("file ends in the middle of a quoted token")
            END
          END;
          Push(s, p, isCurr)
        END;
        INC(p)
      END;

      RETURN c
    END DoNames;

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

    PROCEDURE GetInt(VAR z : INTEGER) : CARDINAL
      RAISES { ShortRead } =
      (* returns length of string read *)
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
        RETURN  p - s
      END GetInt;

    PROCEDURE GetLR(VAR z : LONGREAL) : BOOLEAN
      RAISES { ShortRead, SyntaxError } =
      VAR  
        len : CARDINAL;
        fl : INTEGER;
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
          fl := m;
          m := ABS(m);
          neg := m = -fl;
          Get('.');
          len := GetInt(m);
          Get('e');
          EVAL GetInt(x);

          WITH tabIdx = x - len DO
            IF tabIdx >= FIRST(Exp) AND tabIdx <= LAST(Exp) THEN
              z := FLOAT(m,LONGREAL) * Exp[tabIdx]
            ELSE
              (*
              RAISE SyntaxError(F("scientific notation out of range: tabIdx=%s first=%s last=%s intExponent=%s",
                                  Int(tabIdx),
                                  Int(FIRST(Exp)),
                                  Int(LAST(Exp)),
                                  Int(x)))
              *)
              z := FLOAT(m, LONGREAL) * Math.pow(10.0d0,
                                                 FLOAT(x - len, LONGREAL))
            END;

          END;
          
          IF neg THEN z := -z END;
          IF doDebug THEN 
            Debug.Out(F("GetLR %s -> %s x (%s - %s) -> %s",
                        Int(fl), Int(m), Int(x), Int(len),
                        LongReal(z)), 1000)
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
          IF lbp # 0 AND lbq # aNodes THEN
            Debug.Warning(F("Short read on line %s : got %s # expected %s values (ZEROING)",
                            Int(lNo), Int(lbq), Int(aNodes)));
            FOR i := lbq TO aNodes - 1 DO
              lbuff[lbp, i] := 0.0d0
            END
          END;
          INC(lbp);
          
          IF lbp = NUMBER(lbuff[0]) THEN
            <*ASSERT lbq = aNodes*>
            FlushData(wdWr^, lbp, lbq, names, lbuff^);
            lbp := 0
          END;
          rbq := 0;
          lbq := 0;

          (* process time timestamp -- never optional *)
          <*ASSERT idxMap.get(rbq) # NameControl.NoMapping*>
          TRY
            EVAL GetLR(lbuff[lbq, lbp]);
          EXCEPT
            SyntaxError(e) => RAISE SyntaxError("Getting timestamp : " & e)
          END;
          lbuff[lbq, lbp] := timeScaleFactor * (lbuff[lbq,lbp] + timeOffset);
          IF doDebug THEN
            Debug.Out(F("time %s", LongReal(lbuff[lbq,lbp])))
          END;
          INC(lbq); INC(rbq); (* move to next *)
          
          EVAL GetInt(dummy); (* should really assert this is = names.size() *)
        END;
        
        <*ASSERT rbq >= lbq*>
        TRY
          WHILE GetLR(z) DO

            <*ASSERT rbq >= lbq*>
            
            IF idxMap.get(rbq) # NameControl.NoMapping THEN
              IF idxMap.get(rbq) # lbq THEN
                Debug.Error(F("Internal error: idxMap.get(rbq %s) %s # lbq %s",
                              Int(rbq),
                              Int(idxMap.get(rbq)),
                              Int(lbq)))
              END;

              lbuff[lbq,lbp] := z * voltageScaleFactor + voltageOffset;
              INC(lbq); INC(got)
            END;
            INC(rbq)
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
          <*ASSERT lbq = aNodes*>
          FlushData(wdWr^, lbp, lbq, names, lbuff^)
        END;
        RAISE ShortRead
      END
    END DoData;
    
  VAR
    buf  : ARRAY [0..8191] OF CHAR;
    wdWr : REF ARRAY OF Wr.T;
    start : CARDINAL;
    first : BOOLEAN;

    parser := ParseControl.Null;

    gotNames := FALSE;
    
  TYPE
    ParseControl = { Null, Data, Names };

  PROCEDURE GetSubLine(rd : Rd.T; VAR buf : ARRAY OF CHAR) : CARDINAL
    RAISES { Rd.Failure, Thread.Alerted } =
    BEGIN
      IF wait THEN
        VAR
          ptr := 0;
        BEGIN
          WHILE ptr = 0 OR buf[ptr - 1] # '\n' DO
            IF ptr = NUMBER(buf) THEN
              Debug.Error("line too long")
            END;
            WITH got = Rd.GetSubLine(rd,
                                     SUBARRAY(buf, ptr, NUMBER(buf) - ptr)) DO
              INC(ptr, got);
              IF got = 0 THEN Thread.Pause(0.1d0) END
            END
          END;
          <* ASSERT ptr # 0 *>
          RETURN ptr
        END
      ELSE
        RETURN Rd.GetSubLine(rd, buf)
      END
    END GetSubLine;
    
  PROCEDURE DoLine() : BOOLEAN
    RAISES { ShortRead, SyntaxError, Rd.Failure } =
    BEGIN
      WITH n = GetSubLine(rd, buf) DO

        IF n = NUMBER(buf) THEN 
          Debug.Error("line too long")
        ELSIF NOT wait AND n = 0 THEN
          IF Rd.EOF(rd) THEN
            IF lbp # 0 THEN

              (* we do NOT throw away the last value here, unless we have a
                 short read (can we get here with a short read?) *)

              IF lbq # NUMBER(lbuff^) THEN DEC(lbp) END;
              
              Debug.Out(F("Hit EOF case lbp %s lbq %s", Int(lbp), Int(lbq)));
              
              lbq := NUMBER(lbuff^);
              FlushData(wdWr^, lbp, lbq, names, lbuff^)
            END;
            Rd.Close(rd);
            RETURN FALSE
          END
        ELSE 
          WITH line = SUBARRAY(buf,0,n) DO
            IF doDebug THEN
              Debug.Out("line " & Text.FromChars(line), 1000)
            END;
            start := 0;
            first := FALSE;

            IF    StartsWith(line,ARRAY OF CHAR { '#' }) THEN
              first := TRUE;

              IF    StartsWith(line,ARRAY OF CHAR { '#', 'N' }) THEN
                (* this is a line of node names, parse it with DoNames *)
                parser := ParseControl.Names
              ELSIF StartsWith(line,ARRAY OF CHAR { '#', 'C' }) THEN

                Debug.Out("timestep");
                
                (* this is a timestep, parse it with DoData

                   BUT

                   if the last line we parsed was the last line of N,
                   above, then we need to write the .names file for aplot 
                *)
                IF parser = ParseControl.Names THEN
                  (* must write out names before we forget! *)
                  idxMap := MakeIdxMap(names, restrictNodes, restrictRegEx);
                  
                  aNodes := WriteNames(wd,
                                       ofn,
                                       names,
                                       idxMap,
                                       maxFiles,
                                       nFiles,
                                       wdWr);

                  gotNames := TRUE;

                  WITH n = aNodes,
                       l = BYTESIZE(LONGREAL),
                       q = MaxMem DIV (n * l) DO
                    lbuff := NEW(REF ARRAY OF ARRAY OF LONGREAL, n, q)
                  END;
                END;

                (* now parse the timestep data *)
                parser := ParseControl.Data
              ELSIF StartsWith(line,ARRAY OF CHAR { '#', ';' }) THEN
                (* this is the last line of the file, skip it *)
                parser := ParseControl.Null;

                IF wait THEN
                  IF lbp # 0 THEN

                    (* lop off the last value if for some reason we have a
                       short read... but this should never happen! *)
                    IF lbq # NUMBER(lbuff^) THEN
                      Debug.Warning("values missing on FINAL timestep");
                      DEC(lbp)
                    END;

                    lbq := NUMBER(lbuff^);
                    FlushData(wdWr^, lbp, lbq, names, lbuff^)
                  END;
                  Rd.Close(rd);

                  RETURN FALSE
                END;
              END;
              start := 2
            END;

            WITH str = SUBARRAY(line, start, n - start) DO
              CASE parser OF
                ParseControl.Null =>
                EVAL NullParser(str, first)
              |
                ParseControl.Data =>
                EVAL DoData(str, first)
              |
                ParseControl.Names =>
                EVAL DoNames(str, first)
              END
            END
          END
        END
      END;
      
      IF lNo MOD DebugStep = 0 THEN
        Debug.Out(F("line %s", Int(lNo)))
      END;
      INC(lNo);
      RETURN TRUE
    END DoLine;

  VAR
    idxMap : CardSeq.T;
    aNodes : CARDINAL;
  BEGIN
    (* assumed file structure:

       <stuff to ignore>
       #N <node names, may be multiline>
       #C <timestep data, may be multiline, same # of cols as nodenames>
       #C <repeated>
       #;
       EOF

    *)
    TRY
      WHILE DoLine() DO END;

    FINALLY

      (* sanitize names -- remove names not used *)
      SanitizeNames(idxMap, names);
    
      Debug.Out("Tr0.Parse closing temp files.");

      IF NOT gotNames THEN
        Debug.Error("Tr0.Parse: no (not enough?) data found")
      END;
      
      TRY
        <*ASSERT wdWr # NIL*>
        FOR i := FIRST(wdWr^) TO LAST(wdWr^) DO
          <*ASSERT wdWr[i] # NIL*>
          Wr.Close(wdWr[i])
        END;
      EXCEPT
        Wr.Failure(x) => Debug.Error("Trouble closing temp files : Wr.Failure : " &
          AL.Format(x))
      END;
      Debug.Out("Tr0.Parse temp files closed.")
      
    END

  END Parse;

CONST
  DebugStep = 1000 * 1000;
  
VAR
  Exp : ARRAY [-300..300] OF LONGREAL;

BEGIN

  FOR i := FIRST(Exp) TO LAST(Exp) DO
    Exp[i] := Math.pow(10.0d0, FLOAT(i, LONGREAL))
  END;

END Tr0.
