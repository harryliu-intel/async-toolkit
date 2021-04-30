(* $Id$ *)

MODULE Main;

(* cgidb <httpd-log> <outBase> *)

(* CGI debugging program for things using the htmltable system. *)

IMPORT Params;
IMPORT Text, Rd;
FROM Text IMPORT Sub;
IMPORT Process, TextTextTbl, FileRd;
IMPORT Thread;
IMPORT Wr, FileWr;
IMPORT OSError;
FROM Fmt IMPORT F;
IMPORT AL;
IMPORT Debug;
IMPORT Stdio;

<* FATAL Thread.Alerted *>

PROCEDURE Search(in : TEXT; for : TEXT;
                 VAR beg, end : CARDINAL; 
                 start : CARDINAL := 0) : BOOLEAN =
  VAR 
    p : CARDINAL;
  BEGIN
    (* brute-force text matching *)
    WITH len  = Text.Length(in),
         flen = Text.Length(for) DO
      p := start;

      WHILE p < len DO
        IF Text.Equal(Text.Sub(in,p,flen),for) THEN
          beg := p; end := p + flen;
          RETURN TRUE
        END;
        INC(p)
      END
    END;
    
    RETURN FALSE
  END Search;

PROCEDURE Matches(in, for : TEXT) : BOOLEAN =
  VAR 
    dbeg, dend : CARDINAL;
  BEGIN 
    RETURN Search(in,for,dbeg,dend)
  END Matches;

PROCEDURE Split2(line : TEXT;
                 s1 : TEXT;
                 VAR x1 : TEXT;
                 s2 : TEXT;
                 VAR x2 : TEXT;
                 s3 : TEXT) : BOOLEAN =
  VAR
    l0, l1, l2, l3, l4, l5 : CARDINAL;
  BEGIN

    (*
             TAG1           TAG2            TAG3 
            l0  l1         l2  l3          l4  l5
     *)

    
    IF NOT Search(line,s1,l0,l1) THEN RETURN FALSE END;

    IF NOT Search(line,s2,l2,l3,l1) THEN RETURN FALSE END;

    IF NOT Search(line,s3,l4,l5,l3) THEN RETURN FALSE END;

    IF l2-l1 < 2 OR l4-l3 < 2 THEN RETURN FALSE END;

    x1 := Sub(line,l1,l2-l1);

    x2 := Sub(line,l3,l4-l3);

    RETURN TRUE
  END Split2;

PROCEDURE Split1(line : TEXT;
                 s1 : TEXT;
                 VAR x : TEXT;
                 s2 : TEXT) : BOOLEAN =
  VAR
    l0, l1, l2, l3 : CARDINAL;
  BEGIN
    IF NOT Search(line,s1,l0,l1) THEN RETURN FALSE END;

    IF NOT Search(line,s2,l2,l3,l1) THEN RETURN FALSE END;

    IF l2-l1 < 2 THEN RETURN FALSE END;

    x := Sub(line,l1,l2-l1);
    
    RETURN TRUE
  END Split1;

PROCEDURE GetLineBackwards(rd : Rd.T) : TEXT RAISES { Rd.EndOfFile, Rd.Failure } =
  (* get the next line going backwards to the next \n before and leave
     the cursor at the carriage return *)
  VAR
    p : CARDINAL;
  BEGIN

    WITH start = Rd.Index(rd)-1 DO
      IF start = -1 THEN RAISE Rd.EndOfFile END;
      p := start+1;
      
      LOOP
        IF p = 0 THEN Rd.Seek(rd,0); EXIT END;
        Rd.Seek(rd,p-1);
        IF Rd.GetChar(rd) = '\n' THEN EXIT END;
        DEC(p)
      END;

      (* we are now located at p, beginning of the thing we want to read *)
      BEGIN
        WITH res = GetChars(rd,start-p+1) DO
          Rd.Seek(rd,p-1) (* seek to CR *) ;
          RETURN res
        END
      END
    END
  END GetLineBackwards;

PROCEDURE GetChars(rd : Rd.T; num : CARDINAL) : TEXT RAISES { Rd.EndOfFile, 
                                                              Rd.Failure } =
  BEGIN
    WITH start = Rd.Index(rd),
         buff = NEW(REF ARRAY OF CHAR, num) DO
      TRY
        IF Rd.GetSub(rd,buff^) # num THEN RAISE Rd.EndOfFile END;
        RETURN Text.FromChars(buff^)
      FINALLY
        Rd.Seek(rd,start)
      END
    END
  END GetChars;
  
VAR
  doPath := FALSE;
  outBase : TEXT; 
  httpdLog : TEXT;
  matchString : TEXT := NIL;
  rd : Rd.T;
  startFound, parsing := FALSE;
  env := NEW(TextTextTbl.Default).init();
  pageInput : TEXT := NIL;
BEGIN

  IF Params.Count # 3 THEN
    Wr.PutText(Stdio.stderr, F("Usage: %s <httpd-log> <outBase>\n",
                               Params.Get(0)));
    Process.Exit(1)
  END;
  
  outBase := Params.Get(2);
  httpdLog := Params.Get(1);
  
  TRY
    rd := FileRd.Open(httpdLog);
  EXCEPT
    OSError.E(x) => Debug.Error(F("Trouble opening httpdLog \"%s\" : OSError.E : %s",
                                  httpdLog,
                                  AL.Format(x)))
  END;
  
  (* read the httpdLog backwards *)
  <* FATAL Rd.Failure *>
  BEGIN
    WITH len = Rd.Length(rd) DO Rd.Seek(rd,len-1) END
  END;

  IF Params.Count >= 4 THEN
    matchString := Params.Get(3)
  END;
   
  TRY
    LOOP
      VAR
        line := GetLineBackwards(rd);
        k, v : TEXT;
      BEGIN
        IF matchString = NIL OR Matches(line, matchString) THEN
          IF Matches(line, " ENDPAGE.") THEN
            parsing := TRUE
          ELSIF parsing THEN
            IF Matches(line, " STARTPAGE:") THEN
              startFound := TRUE;
              EXIT
            ELSIF Matches(line, " ENDPAGE.") THEN
              Process.Crash("Found two ENDPAGEs without an intervening STARTPAGE.")
            ELSIF Split2(line, " PAGEDATAKEY ", k, 
                         " PAGEDATAVALUE ", v, " ENDPAGEDATA;") THEN
              EVAL env.put(k,v)
            ELSIF Split1(line, " PAGEINPUT ", v, " ENDPAGEINPUT;") THEN
              pageInput := v
            END
          END
        END
      END
    END
  EXCEPT
    Rd.Failure(x) => Debug.Error(F("Trouble reading httpdLog Rd.Failure : %s",
                                  AL.Format(x)))
  |
    Rd.EndOfFile => (* skip *)
  END;

  IF NOT startFound THEN
    Process.Crash("No STARTPAGE found!")
  ELSIF pageInput = NIL THEN
(*    Process.Crash("pageInput NIL!") *)
  END;

  TRY
    WITH wr = FileWr.Open(outBase & ".csh"),
         iter = env.iterate() DO
      VAR k,v : TEXT; BEGIN
        WHILE iter.next(k,v) DO
          IF doPath OR NOT Text.Equal(k,"PATH") THEN
            Wr.PutText(wr, "setenv ");
            Wr.PutText(wr, k);
            Wr.PutText(wr, " \"");
            Wr.PutText(wr, v);
            Wr.PutText(wr, "\"\n")
          END
        END
      END;
      Wr.Close(wr)
    END;
  EXCEPT
    OSError.E(x) => Debug.Error(F("Trouble opening/closing \"%s\" for writing : OSError.E : %s",
                                  outBase & ".csh",
                                  AL.Format(x)))
  |
    Wr.Failure(x) => Debug.Error(F("Trouble writing \"%s\"  : Wr.Failure : %s",
                                  outBase & ".csh",
                                  AL.Format(x)))
  END;

  TRY
    WITH wr = FileWr.Open(outBase & ".in") DO
      IF pageInput # NIL THEN
        Wr.PutText(wr, pageInput)
      END;
      Wr.Close(wr)
    END
  EXCEPT
    OSError.E(x) => Debug.Error(F("Trouble opening/clsoing \"%s\" for writing : OSError.E : %s",
                                  outBase & ".in",
                                  AL.Format(x)))
  |
    Wr.Failure(x) => Debug.Error(F("Trouble writing \"%s\"  : Wr.Failure : %s",
                                  outBase & ".in",
                                  AL.Format(x)))
  END

END Main.
