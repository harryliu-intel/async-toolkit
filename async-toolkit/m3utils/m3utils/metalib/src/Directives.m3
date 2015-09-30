MODULE Directives;
IMPORT Rd;
FROM Lexer IMPORT GetToken, String, State, BufSize;
IMPORT Name, Text, Dsim, Debug, NodePair;
IMPORT NameSet;
<*NOWARN*>IMPORT IO, Fmt;

<*NOWARN*>PROCEDURE D(what : TEXT) = BEGIN (*IO.Put(what & "\n")*) END D;

PROCEDURE Parse(rd          : Rd.T; 
                globalNames : NameSet.T;
                prefix      : Name.T;
                res         : Table) =

  PROCEDURE Error() =
    BEGIN
      Debug.Error("PARSE ERROR, lately reading: " & Text.FromChars(SUBARRAY(buff,token.start,token.n)))
    END Error;

  PROCEDURE Next() =
    BEGIN eop := NOT GetToken(buff,state,token) END Next;

  PROCEDURE GetExactChar(c : CHAR) : BOOLEAN =
    BEGIN 
      IF token.n # 1 OR
        SUBARRAY(buff,token.start,token.n)[0] # c THEN
        RETURN FALSE
      ELSE
        Next();
        D("ExactChar " & Text.FromChar(c)); RETURN TRUE
      END
    END GetExactChar;

  PROCEDURE GetCharFromSet(s : SET OF CHAR; VAR c : CHAR) : BOOLEAN =
    BEGIN
      IF token.n # 1 OR
        NOT SUBARRAY(buff,token.start,token.n)[0] IN s THEN
        RETURN FALSE
      ELSE
        c := SUBARRAY(buff,token.start,token.n)[0];
        Next();
        D("CharFromSet");
        RETURN TRUE
      END
    END GetCharFromSet;

  PROCEDURE GetExactText(t : TEXT) : BOOLEAN =
    VAR l := Text.Length(t);
    BEGIN
(*
      IO.Put("GetExactText(\"" & t & "\") : buff=\"" &
        Text.FromChars(SUBARRAY(buff,token.start,token.n)) & "\", l=" & 
        Fmt.Int(l) & " token.n=" & Fmt.Int(token.n) & "\n");
*)

      IF token.n # l THEN 
        RETURN FALSE
      ELSE
        FOR i := 0 TO l-1 DO
          IF buff[token.start+i] # Text.GetChar(t,i) THEN RETURN FALSE END
        END;
        Next();
        D("ExactText " & t); RETURN TRUE
      END
    END GetExactText;

  CONST 
    DownChar = '-';
    UpChar   = '+';
  CONST
    DirSet = SET OF CHAR { DownChar, UpChar };

  PROCEDURE GetName(VAR name : Name.T) : BOOLEAN =
    (* get + or - terminated name *)
    CONST 
      MaxName = 2048;
    VAR
      array : ARRAY [0..MaxName-1] OF CHAR;
      p := 0;
    BEGIN 
      (* tricky, because names arent quoted in this format *)
      
      WHILE NOT buff[token.start] IN DirSet DO
        SUBARRAY(array,p,token.n) := SUBARRAY(buff,token.start,token.n);
        INC(p,token.n);
        Next();
      END;
      name := Name.ParseChars(SUBARRAY(array,0,p));
      D("Name"); RETURN TRUE
    END GetName;

  PROCEDURE PrefixName(VAR name : Name.T) =
    BEGIN
      IF prefix # NIL AND NOT globalNames.member(name) THEN
        name := Name.Append(prefix,name)
      END
    END PrefixName;
        
  PROCEDURE GetParenthesizedNodePair(VAR pair : NodePair.T) : BOOLEAN =

    VAR
      inDirChar, outDirChar : CHAR;

    PROCEDURE Convert(c : CHAR) : Dsim.Sense =
      BEGIN
        CASE c OF 
          DownChar => RETURN Dsim.Sense.Down
        |
          UpChar => RETURN Dsim.Sense.Up
        ELSE
          Error(); <*ASSERT FALSE*>
        END
      END Convert;

    BEGIN
      IF NOT GetExactChar('(') THEN 
        RETURN FALSE
      ELSE
        IF NOT GetExactChar('{')    OR
           NOT GetName(pair.fanout) OR
           NOT GetCharFromSet(DirSet,outDirChar) OR
           NOT GetExactChar(',')    OR
           NOT GetName(pair.fanin)  OR           
           NOT GetCharFromSet(DirSet,inDirChar) OR
           NOT GetExactChar('}')    OR
           NOT GetExactChar(')')       THEN
          Error()
        END;
        PrefixName(pair.fanout);
        PrefixName(pair.fanin);
        pair.outDir := Convert(outDirChar);
        pair.inDir := Convert(inDirChar);
        D("ParenthesizedNodePair"); RETURN TRUE
      END
    END GetParenthesizedNodePair;

  PROCEDURE GetCard(VAR c : CARDINAL) : BOOLEAN =
    VAR res := 0;
    BEGIN
      FOR i := token.start TO token.start+token.n-1 DO
        WITH num = ORD(buff[i])-ORD('0') DO
          IF num < 0 OR num > 9 THEN RETURN FALSE END;
          res := 10*res+num
        END
      END;
      c := res;
      Next();
      D("Card"); RETURN TRUE
    END GetCard;

  PROCEDURE GetLongReal(VAR lr : LONGREAL) : BOOLEAN =
    VAR 
      p := token.start;
      mult := LAST(LONGREAL);
      res := 0.0d0;
      neg := FALSE;
    BEGIN
      IF token.n = 0 THEN RETURN FALSE END; (* shouldnt happen in any case *)
      IF buff[p] = '-' THEN neg := TRUE; INC(p) END;
      WHILE p < token.start + token.n DO
        IF buff[p]='.' THEN 
          IF mult # LAST(LONGREAL) THEN RETURN FALSE END;
          mult := 0.1d0 
        ELSE
          WITH d = ORD(buff[p]) - ORD('0') DO
            IF d < 0 OR d > 9 THEN RETURN FALSE END;
            IF mult = LAST(LONGREAL) THEN
              res := res*10.0d0 + FLOAT(d,LONGREAL)
            ELSE
              res := res + mult * FLOAT(d,LONGREAL);
              mult := mult * 0.1d0
            END
          END
        END;
        INC(p)
      END;
      IF neg THEN
        lr := -res
      ELSE
        lr := res
      END;
      Next();
      D("LongReal"); RETURN TRUE
    END GetLongReal;

  PROCEDURE GetCSV(VAR data : ARRAY OF LONGREAL; 
                   VAR len : [-1..LAST(CARDINAL)]) : BOOLEAN =
    BEGIN
      IF NOT GetExactChar('{') THEN
        RETURN FALSE
      ELSE
        VAR ptr := 0; BEGIN
          LOOP
            IF GetLongReal(data[ptr]) THEN
              INC(ptr);
              IF NOT GetExactChar(',') THEN EXIT END
            ELSE
              EXIT
            END
          END;
          IF NOT GetExactChar('}') THEN 
            Error() ; <*ASSERT FALSE*>
          ELSE
            IF    len = -1  THEN len := ptr 
            ELSIF len # ptr THEN Error() 
            END;
            RETURN TRUE
          END
        END
      END
    END GetCSV;

  PROCEDURE GetMeasuredDelayBlock(VAR block : DelayBlock) : BOOLEAN =
    CONST MaxCsvLen = 100;
    VAR
      vv   : ARRAY Data OF ARRAY [0..MaxCsvLen-1] OF LONGREAL;
      l    : [-1..LAST(CARDINAL)] := -1;
      mode : CARDINAL;
    BEGIN
      IF NOT GetExactText("measured_delay") THEN 
        RETURN FALSE
      ELSE
        block := NEW(DelayBlock);
        IF NOT GetParenthesizedNodePair(block.nodes) OR
           NOT GetExactChar('=')                     OR
           NOT GetExactChar('{')                     OR
           NOT GetExactChar('{')                     OR
           NOT GetCard(mode)                         OR
           NOT GetExactChar('}')                     OR
           mode # 0                                       THEN
          Error()
        END;

        FOR d := FIRST(Data) TO LAST(Data) DO
          IF NOT GetExactChar(',') OR
             NOT GetCSV(vv[d],l) THEN 
            Error() 
          END
        END;

        IF  NOT GetExactChar('}')                     OR
            NOT GetExactChar(';')                         THEN
          Error()
        END;

        block.data := NEW(REF ARRAY OF ARRAY Data OF LONGREAL, l);
        
        FOR i := 0 TO l-1 DO
          FOR j := FIRST(Data) TO LAST(Data) DO
            block.data[i][j] := vv[j][i]
          END
        END;
        D("MeasuredDelayBlock"); RETURN TRUE
      END
    END GetMeasuredDelayBlock;

  VAR
    buff : ARRAY [0..BufSize-1] OF CHAR;
    eop  := FALSE; (* done parsing *)
    token : String;
    state  : State;
    block  : DelayBlock;
  BEGIN
    state.rd := rd;

    Next(); (* lookahead *)
    WHILE GetMeasuredDelayBlock(block) DO
      EVAL res.put(block.nodes,block)
    END;

    IF NOT eop THEN Error() END
  END Parse;

BEGIN END Directives.
