MODULE Main;
IMPORT Pathname;
IMPORT FileRd, Rd;
IMPORT CSVParse;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal, Pad;
IMPORT Text;
IMPORT TextUtils;
IMPORT LongRealSeq;
IMPORT FloatMode, Lex;
IMPORT Scan;
IMPORT Thread;
IMPORT OSError;

<*FATAL Thread.Alerted*>
<*FATAL OSError.E, Rd.Failure*>

CONST TE = Text.Equal;
CONST Verbose = FALSE;
      
PROCEDURE ReadTableFrom(READONLY col, row : CARDINAL) =
  VAR
    vseq, tseq := NEW(LongRealSeq.T).init();
    r, c : CARDINAL;
    str : TEXT;
    data : REF ARRAY OF ARRAY OF LONGREAL;
  BEGIN
    Debug.Out(F("Reading table from magic at %s %s", Int(col), Int(row)));

    (* get col and row headings *)

    TRY
      c := col + 1;
      LOOP
        str := Get(c,row,"**EMPTY**");
        IF Verbose THEN Debug.Out("parsing T " & str) END;
        WITH nxt = Scan.LongReal(str) DO
          tseq.addhi(nxt);
          Debug.Out("temp  " & LongReal(nxt));
        END;
        INC(c)
      END
    EXCEPT
      Lex.Error, FloatMode.Trap => (* skip *)
    END;

    TRY
      r := row + 1;
      LOOP
        str := Get(col,r,"**EMPTY**");
        IF Verbose THEN Debug.Out("parsing V " & str) END;
        WITH nxt = Scan.LongReal(str) DO
          vseq.addhi(nxt);
          Debug.Out("volts " & LongReal(nxt))
        END;
        INC(r)
      END
    EXCEPT
      Lex.Error, FloatMode.Trap => (* skip *)
    END;

    (* last c, r were first invalid T, V values *)
    (* data runs from col+1..c-1, row+1..r-1 *)
    
    (* fill in data *)
    VAR
      lst : TEXT;
      lsti, lstj : CARDINAL;
    BEGIN
      data := NEW(REF ARRAY OF ARRAY OF LONGREAL,
                  c-1-col, r-1-row);
      TRY
        FOR i := col+1 TO c-1 DO
          FOR j := row+1 TO r-1 DO
            lsti := i;
            lstj := j;
            lst := Get(i,j,"**ERROR**");
            data[i-col-1,j-row-1] := Scan.LongReal(lst);
          END
        END
      EXCEPT
        FloatMode.Trap, Lex.Error =>
        Debug.Error(F("error at %s %s : %s", Int(lsti), Int(lstj), lst))
      END
    END;

    (* now search for headers *)
    VAR
      bars := 0;
      r : [0..LAST(CARDINAL) ] := row-1;
      hcnt : CARDINAL := 0;
    CONST
      Done = "**DONE**";
      QS = SET OF CHAR { '"' };
    BEGIN
      LOOP
        WITH cell = TextUtils.FilterOut(Get(col, r, Done),QS)
         DO
          Debug.Out(F("Check header cell %s", cell));
          IF TE(cell,Done) THEN
            EXIT
          ELSIF IsBar(cell) THEN
            Debug.Out(F("bar at %s %s", Int(col), Int(r)));
            INC(bars)
          ELSIF NOT IsWhiteSpace(cell) THEN
            ParseHeaderCell(cell, hcnt)
          END
        END;
        
        IF bars = 2 THEN EXIT END;

        DEC(r)
      END
    END
    
  END ReadTableFrom;

CONST WS = SET OF CHAR { ' ', '\t', '\n', '\r' };
      
PROCEDURE IsBar(txt : TEXT) : BOOLEAN =
  VAR
    mayBeBar := FALSE;
  BEGIN
    FOR i := 0 TO Text.Length(txt)-1 DO
      WITH c = Text.GetChar(txt,i) DO
        IF c = '#' THEN
          mayBeBar := TRUE 
        ELSIF NOT c IN WS THEN
          RETURN FALSE
        END;
      END
    END;
    RETURN mayBeBar
  END IsBar;

PROCEDURE IsWhiteSpace(txt : TEXT) : BOOLEAN =
  BEGIN
    RETURN Text.Length(TextUtils.FilterOut(txt, WS)) = 0
  END IsWhiteSpace;

PROCEDURE ParseHeaderCell(txt : TEXT; VAR cnt : CARDINAL) =
  VAR
    beg, end : TEXT;
  BEGIN
    (* equals separated *)
    IF TextUtils.CountCharOccurences(txt, '=') = 1 THEN
      TextUtils.SplitText(txt, '=', beg, end);
      beg := TextUtils.FilterOut(beg, WS);
      end := TextUtils.FilterOut(end, WS);
      Debug.Out(F("binding %s :=: %s", beg, end));
      RETURN
    END;

    (* not comma separated *)
    IF TextUtils.CountCharOccurences(txt, ',') = 0 THEN
      TextUtils.SplitText(txt, ' ', beg, end);
      beg := TextUtils.FilterOut(beg, WS);
      end := TextUtils.FilterOut(end, WS);
      Debug.Out(F("binding %s :=: %s", beg, end));
      RETURN
    END;
    
    (* comma separated *)
    VAR lst := TextUtils.Shatter(txt,",");
        p := lst;
    BEGIN
      WHILE p # NIL DO
        beg := "PARAM" & Pad(Int(cnt), 4, '0');
        end := p.head;
        end := TextUtils.FilterOut(end, WS);
        Debug.Out(F("binding %s :=: %s", beg, end));
        p := p.tail;
        INC(cnt)
      END
    END;
  END ParseHeaderCell;
      
PROCEDURE Do(rd : Rd.T) =
  VAR
    parser := NEW(CSVParse.T).init(rd);
    cell : TEXT;
    r, c : CARDINAL := 0;
  BEGIN
    arr := NEW(REF ARRAY OF ARRAY OF TEXT, 1, 1);
    arr[0,0] := NIL;
    (* read in the whole sheet *)
    TRY
      LOOP
        Debug.Out("==================    LINE    ==================");
        parser.startLine();
        WHILE parser.cellB(cell, handleQuotes := TRUE) DO
          Debug.Out("cell " & cell);
          Set(c, r, cell);
          INC(c)
        END;
        c := 0;
        INC(r)
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;

    (* now search for the magic string *)
    CONST
      Magic = "V/T";
    BEGIN
      FOR c := FIRST(arr^) TO LAST(arr^) DO
        FOR r := FIRST(arr[0]) TO LAST(arr[0]) DO
          IF arr[c,r] # NIL AND TE(arr[c,r],Magic) THEN
            ReadTableFrom(c, r)
          END
        END
      END
    END          
  END Do;

CONST Files = ARRAY OF Pathname.T { "8L.csv", "11L.csv" };
CONST Pfx = "../data";
  
VAR
  rd : ARRAY [0..NUMBER(Files)-1] OF Rd.T;
  arr : REF ARRAY OF ARRAY OF TEXT;

PROCEDURE Get(col, row : INTEGER; empty : TEXT) : TEXT =
  BEGIN
    IF col < 0 OR row < 0 THEN
      RETURN empty
    END;
    
    IF col > LAST(arr^) THEN
      RETURN empty
    ELSIF row > LAST(arr[0]) THEN
      RETURN empty
    ELSIF arr[col,row] = NIL THEN
      RETURN empty
    ELSE
      RETURN arr[col,row]
    END
  END Get;
  
PROCEDURE Set(col, row : CARDINAL; to : TEXT) =
  (* to match excel, columns first, rows second *)
  VAR
    newCols := NUMBER(arr^);
    newRows := NUMBER(arr[0]);
  BEGIN
    IF Verbose THEN Debug.Out(F("set %s %s <- %s", Int(col), Int(row), to)) END;
    IF col > LAST(arr^) THEN
      newCols := MAX(2*LAST(arr^),col + 1);
    END;
    IF row > LAST(arr[0]) THEN
      newRows := MAX(2*LAST(arr[0]), row + 1);
    END;
    IF newCols # NUMBER(arr^) OR newRows # NUMBER(arr[0]) THEN
     WITH new = NEW(REF ARRAY OF ARRAY OF TEXT, newCols, newRows) DO
        FOR i := FIRST(new^) TO LAST(new^) DO
          FOR j := FIRST(new[0]) TO LAST(new[0]) DO
            IF i <= LAST(arr^) AND j <= LAST(arr[0]) THEN
              new[i,j] := arr[i,j]
            ELSE
              new[i,j] := NIL
            END
          END
        END;
        arr := new
      END
    END;
    arr[col, row] := to
  END Set;
  
BEGIN
  
  FOR i := FIRST(Files) TO LAST(Files) DO
    rd[i] := FileRd.Open(Pfx & "/" & Files[i])
  END;

  Do(rd[0]);
  Do(rd[1]);
END Main.
