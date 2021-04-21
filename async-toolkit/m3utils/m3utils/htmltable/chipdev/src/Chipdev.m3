(* $Id$ *)

MODULE Chipdev;
IMPORT DBerr;
IMPORT HTML, HTMLPage, HTMLForm, HTMLInput, HTMLLink, HTMLList;
IMPORT HTMLTextArea;
IMPORT Pages,Debug,Database, DatabaseUtils, Text, Session, Scan;
IMPORT Fmt;
IMPORT Lex, FloatMode;
IMPORT Request;
IMPORT TextReader;
IMPORT Wr;
IMPORT PageDispatch;
IMPORT HTMLFormatting; FROM HTMLFormatting IMPORT Alignment, RowFormat;
IMPORT Process;
IMPORT HTMLTable;
IMPORT IntSet, IntSetDef, CardSetDef;
IMPORT Thread;
IMPORT XTime AS Time;
IMPORT Pathname, Params;
IMPORT Random, FileWr, FS, OSError;
IMPORT TZ;
IMPORT SysPerf;
IMPORT TextRefTbl;
IMPORT Rd, FileRd;
IMPORT TextUtils;
IMPORT RefList;
IMPORT AL;

CONST TE = Text.Equal;
CONST RefreshInterval = 10*1000;
CONST PrintSQL = FALSE;
CONST TmpDir = "/tmp";
CONST GnuplotPath = "/usr/local/bin/gnuplot";
CONST DoDeleteGnuplotFiles = TRUE;

<* FATAL Thread.Alerted *>

PROCEDURE StartURL() : TEXT = 
  BEGIN 
    RETURN "<a href=\"" & CGIname & "?check_signin\">Start page.</a>" 
  END StartURL;

TYPE 
  FileStat = OBJECT
    pn      : Pathname.T;
    touched : Time.T;
    next : FileStat := NIL;
  END;

  DirStat = OBJECT
    median, last : Time.T;
    recs : CARDINAL;
  END;

  FSStat = OBJECT
    pn : Pathname.T;
    avail : LONGREAL;
    next : FSStat := NIL;
  END;

EXCEPTION FormatError;

PROCEDURE SiteStatus() : HTMLList.T RAISES { DBerr.Error,
                                             OSError.E,
                                             FormatError } =

  PROCEDURE FLR(l : LONGREAL) : TEXT =
    BEGIN
      IF l > 1000.0d0 THEN 
        RETURN Fmt.LongReal(l, prec := 3)
      ELSE
        RETURN Fmt.LongReal(l, style := Fmt.Style.Fix, prec := 2)
      END
    END FLR;

  PROCEDURE FmtLA() : TEXT =
    VAR res := ""; BEGIN
      FOR i := FIRST(loadAvg) TO LAST(loadAvg) DO
        res := res & FLR(loadAvg[i]) & " " 
      END;
      RETURN res
    END FmtLA;

  PROCEDURE SummarizeFeeds(path : Pathname.T) RAISES { OSError.E, 
                                                       FormatError } =
    VAR rd := FileRd.Open(path);
    BEGIN
      TRY
        LOOP
          VAR
            line   := Rd.GetLine(rd);
            reader := NEW(TextReader.T).init(line);
            val    := reader.getLR();
            which  := reader.get();
            path   := reader.nextE("");
            dirStat : DirStat;
            r : REFANY;
          BEGIN
            IF dirStats.get(path,r) THEN
              dirStat := r
            ELSE
              dirStat := NEW(DirStat, 
                             median := FIRST(Time.T),
                             last := FIRST(Time.T),
                             recs := 0);
              EVAL dirStats.put(path,dirStat)
            END;

            IF    TE(which, "MEDIAN") THEN
              dirStat.median := MAX(dirStat.median,val)
            ELSIF TE(which, "LAST") THEN
              dirStat.last := MAX(dirStat.last,val)
            ELSIF TE(which, "RECS") THEN
              dirStat.recs := ROUND(val)
            ELSE
              Debug.Warning("Unknown which \"" & which & "\"")
            END
          END
        END
          
      EXCEPT
        Rd.EndOfFile => TRY Rd.Close(rd) EXCEPT Rd.Failure => (* ??? *) END
      |
        Rd.Failure, FloatMode.Trap, Lex.Error, TextReader.NoMore =>
        RAISE FormatError
      END
    END SummarizeFeeds;

  PROCEDURE P(t : TEXT; n : CARDINAL) : TEXT =
    BEGIN
      RETURN " " & TextUtils.Pluralize(t,n)
    END P;

  PROCEDURE FmtTime(t : Time.T) : HTML.Stuff =
    VAR 
      tot, d := ROUND(now - t);
      delta := "";
    BEGIN
      (* we print the two most significant human-readable time intervals,
         i.e.., days/hours, hours/minutes, minutes/seconds, or seconds only *)
      IF tot >= 86400 THEN
        delta := P("day",d DIV 86400);
        d     := d MOD 86400
      END;

      IF tot >= 3600 THEN
        delta := delta & P("hour",d DIV 3600); d := d MOD 3600
      END;

      IF tot >= 60 AND tot < 86400 THEN
        delta := delta & P("minute",d DIV 60); d := d MOD 60
      END;

      IF               tot < 3600 THEN 
        delta := delta & P("second",d) 
      END;

      RETURN HTMLTable.Vert(TZ.FormatSubsecond(tz,t,simplified:= TRUE),
                            delta)
    END FmtTime;

  CONST 
    MyTZ = "America/New_York";
  VAR
    tz := NEW(TZ.T).init(MyTZ);
    res := NEW(HTMLList.T);
    now := Time.Now();
    loadAvg := ARRAY [0..2] OF LONGREAL { LAST(LONGREAL), .. };
    dirStats := NEW(TextRefTbl.Default).init();
    fsStats : FSStat := NIL;
    fileStats : FileStat := NIL;
    monitor := Database.TExec("select * from monitor;");
  BEGIN
    EVAL SysPerf.GetLoadAvg(loadAvg);

    res.add("<br><br>System status at " & 
      TZ.FormatSubsecond(tz,now,simplified:= TRUE) & 
   "  (timezone=" & MyTZ &"): load averages " & FmtLA());

    FOR i := 0 TO monitor.rows()-1 DO
      WITH r = monitor.getRow(i),
           what = r.getI(1) DO
        IF    TE(what, "stat") THEN
          VAR
            newFS := NEW(FileStat, touched := LAST(Time.T), next := fileStats,
                         pn := r.getI(0));
          BEGIN
            TRY
              newFS.touched := FS.Status(r.getI(0)).modificationTime
            EXCEPT
              OSError.E => (* skip *)
            END;
            fileStats := newFS
          END
        ELSIF TE(what, "summarize") THEN
          SummarizeFeeds(r.getI(0))
        ELSIF TE(what, "filesystem") THEN
          VAR 
            newFS := NEW(FSStat, avail := LAST(LONGREAL), pn := r.getI(0), next := fsStats); 
          BEGIN
            TRY
              newFS.avail := SysPerf.DiskAvail(r.getI(0))
            EXCEPT
              SysPerf.Error => (* skip *)
            END;
            fsStats := newFS
          END
        ELSE
          Debug.Warning("Unknown monitor type \"" & what & "\"")
        END
      END
    END;

    VAR stuff : RefList.T := NIL; BEGIN
      IF dirStats.size() # 0 THEN
        VAR
          tab := NEW(REF ARRAY OF ARRAY OF HTML.T, 1, 3);
          hTab := NEW(HTMLTable.T);
          iter := dirStats.iterate();
          path : TEXT;
          r : REFANY;
        BEGIN
          tab[0,0] := HTMLList.Hack("feed dir.");
          tab[0,1] := HTMLList.Hack("last");
          tab[0,2] := HTMLList.Hack("median");
          EVAL hTab.init(tab);
          
          WHILE iter.next(path,r) DO
            WITH rec = NARROW(r,DirStat),
                 vec = NEW(REF ARRAY OF HTML.T, 3) DO
              vec[0] := HTMLList.Hack(path);
              vec[1] := FmtTime(rec.last);
              vec[2] := FmtTime(rec.median);
              
              hTab.addRow(vec)
            END
          END;
          
          stuff := RefList.Cons(hTab, stuff)
        END
      END;
      
      IF fileStats # NIL THEN
        VAR
          tab := NEW(REF ARRAY OF ARRAY OF HTML.T, 1, 2);
          hTab := NEW(HTMLTable.T);
        BEGIN
          tab[0,0] := HTMLList.Hack("file");
          tab[0,1] := HTMLList.Hack("touched");
          EVAL hTab.init(tab);

          WHILE fileStats # NIL DO
            WITH vec = NEW(REF ARRAY OF HTML.T,2) DO
              vec[0] := HTMLList.Hack(fileStats.pn);
              IF fileStats.touched = LAST(Time.T) THEN
                vec[1] := HTMLList.Hack("ERROR") 
              ELSE
                vec[1] := FmtTime(fileStats.touched)
              END;

              hTab.addRow(vec)
            END;
            fileStats := fileStats.next
          END;

          stuff := RefList.Cons(hTab,stuff)
        END
      END;

      IF fsStats # NIL THEN
        VAR
          tab := NEW(REF ARRAY OF ARRAY OF HTML.T, 1, 2);
          hTab := NEW(HTMLTable.T);
        BEGIN
          tab[0,0] := HTMLList.Hack("filesystem");
          tab[0,1] := HTMLList.Hack("used cap.");
          EVAL hTab.init(tab);

          WHILE fsStats # NIL DO
            WITH vec = NEW(REF ARRAY OF HTML.T,2) DO
              vec[0] := HTMLList.Hack(fsStats.pn);
              IF fsStats.avail = LAST(LONGREAL) THEN
                vec[1] := HTMLList.Hack("ERROR") 
              ELSE
                vec[1] := HTMLList.Hack(
                            Fmt.LongReal(100.0d0 * (1.0d0-fsStats.avail), 
                                     prec := 1, style := Fmt.Style.Fix) & 
                                                 "%")
              END;

              hTab.addRow(vec)
            END;
            fsStats := fsStats.next
          END;

          stuff := RefList.Cons(hTab,stuff)
        END
      END;
      
      IF stuff # NIL THEN
        WITH arr = NEW(REF ARRAY OF ARRAY OF HTML.T, 
                       1,RefList.Length(stuff)) DO
          FOR i := LAST(arr[0]) TO FIRST(arr[0]) BY -1 DO
            arr[0,i] := stuff.head;
            stuff := stuff.tail
          END;

          res.add("<br>");
          res.add(NEW(HTMLTable.T).init(arr, useBorders := FALSE))
        END
      END
    END;

    RETURN res
  END SiteStatus;

PROCEDURE SiteNavBar(request : Request.T) : HTMLList.T =

  PROCEDURE AddLinkTo(named : TEXT; titleP : TEXT := NIL) =
    VAR 
      title : TEXT;
      dispatch := Pages.GetPageDispatch(named);
    BEGIN
      IF dispatch = NIL THEN
        res.add("Link to \"" & named & "\"")
      ELSE
        title := titleP;
        IF title = NIL AND ISTYPE(dispatch,Page) THEN
          title := NARROW(dispatch,Page).title
        END;
        IF title = NIL THEN
          title := "Page \"" & named & "\""
        END;
        res.add(NEW(HTMLLink.T).init(title,named,request))
      END
    END AddLinkTo;

  VAR
    res := NEW(HTMLList.T);
    spacing := "&nbsp;&nbsp;&nbsp;";
  BEGIN
    Debug.Out("Building SiteNavBar...");

    AddLinkTo("holding_exec");
    res.add(spacing);

(*
    AddLinkTo("do_exec");
    res.add(spacing);
*)

    AddLinkTo("state_executed");
    res.add(spacing);

    AddLinkTo("state_rejected");
    res.add(spacing);

    AddLinkTo("do_clear");
    res.add(spacing);

    res.add("<br>");

    AddLinkTo("main");
    res.add(spacing);

    AddLinkTo("profile");
    res.add(spacing);

    AddLinkTo("redirect_holdings","Journal");
    res.add(spacing);

    AddLinkTo("balance_sheet");
    res.add(spacing);

    AddLinkTo("pl_log");
    res.add(spacing);


    IF request.session.getPriv() >= Session.Priv.Admin THEN
      AddLinkTo("admin_main")
    END;

    res.add("<br>");

    RETURN res
  END SiteNavBar;

(**********************************************************************)

PROCEDURE Main(p : Page; request : Request.T) =
  BEGIN
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
  END Main;

PROCEDURE AdminMain(p : Page; request : Request.T) =
  BEGIN
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br><br>Administrator menu:<br>");
    p.page.addToBody(NEW(HTMLLink.T).init("List users.","list_users",request));
    p.page.addToBody("<br>");
    p.page.addToBody(NEW(HTMLLink.T).init("Run SQL query.","run_sql",request));
    p.page.addToBody("<br>");
  END AdminMain;

PROCEDURE Signin(p : Page; request : Request.T) =
  VAR
    form := NEW(HTMLForm.T).init("check_signin",request);
  BEGIN
    form.add("<p>Registered email address:");
    form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, "login"));
    form.add("<p>Password:");
    form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, "password"));
    form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit));
    EVAL p.page.setHead("Testing Form...").setBody(form);
  END Signin;

(* run any SQL query *)
PROCEDURE RunSQL(p : Page; request : Request.T) 
  RAISES { DBerr.Error } =
  VAR
    form := NEW(HTMLForm.T).init("run_sql",request);
    query : TEXT;
  BEGIN
    form.add("<p>SQL Query:<br><p>");
    form.add(NEW(HTMLTextArea.T).init("SQL query"));
    form.add("<br>");
    form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit));

    EVAL p.page.setHead("SQL Form...").setBody(form);
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
    IF request.getPostVar("SQL query", query) THEN
      VAR
        result := Database.TExec(query);
        table := result.allColsToHTML();
      BEGIN
        p.page.addToBody(table);
      END
    END;
  END RunSQL;    

TYPE
  MyFormat = HTMLFormatting.Dynamic OBJECT
    request : Request.T;
  OVERRIDES
    format := MFFormat;
  END;
  Color = { Pink, LightSkyBlue };

  MyFormat3 = HTMLFormatting.DynamicTD OBJECT
    request : Request.T;
    in : TEXT := "";
    color := Color.Pink;
  OVERRIDES
    format := ColorFormat;
  END;

CONST ColorNames = ARRAY Color OF TEXT { "pink", "lightskyblue" };

PROCEDURE ColorFormat(m : MyFormat3; in : HTML.Stuff) : HTML.Stuff =
  BEGIN
    IF m.in = NIL THEN m.in := in END;
    (* when in changes, switch colors *)
    IF NOT TE(in,m.in) THEN
      IF m.color = LAST(Color) THEN
        m.color := FIRST(Color)
      ELSE
        INC(m.color)
      END
    END;
    m.in := in;

    WITH list = NEW(HTMLList.T) DO
      IF IsChars(in, SET OF CHAR { '0'..'9' } ) THEN
        list.add("<td align=right bgcolor=" & ColorNames[m.color] & ">");
      ELSE
        list.add("<td>")
      END;
      list.add(in);
      list.add("</td>");
      RETURN list
    END
  END ColorFormat;

PROCEDURE IsChars(txt : TEXT; set : SET OF CHAR) : BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length(txt)-1 DO
      IF NOT Text.GetChar(txt,i) IN set THEN RETURN FALSE END
    END;
    RETURN TRUE
  END IsChars;

PROCEDURE MFFormat(m : MyFormat; in : HTML.Stuff) : HTML.Stuff =
  BEGIN
    TYPECASE in OF 
      TEXT(txt) =>
      IF IsChars(txt, SET OF CHAR { '0'..'9' } ) THEN
        WITH form = NEW(HTMLForm.T).init("do_exec",m.request) DO
          form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit, 
                                         name := "exec",
                                         value := "trade " & txt & "",
                                         style := "color:blue"));
          RETURN form
        END
      ELSE
        RETURN txt
      END
    ELSE
      Process.Crash("Calling MFFormat on non-TEXT!");
      <* ASSERT FALSE *>
    END
  END MFFormat;

TYPE
  MyRowFormat = HTMLFormatting.EveryRowFormat OBJECT
    first := TRUE;
    grey := FALSE;
  OVERRIDES
    bgcolor := MRFbgcolor;
  END;

PROCEDURE MRFbgcolor(mrf : MyRowFormat) : TEXT =
  BEGIN
    IF mrf.first THEN mrf.first := FALSE; RETURN NIL END;

    TRY
      IF mrf.grey THEN
        RETURN "#FFFFCC"
      ELSE
        RETURN "#FFCCFF"
      END
    FINALLY
      mrf.grey := NOT mrf.grey
    END
  END MRFbgcolor;

PROCEDURE RowFormatting() : RowFormat =
  BEGIN
    RETURN NEW(MyRowFormat)
  END RowFormatting;

TYPE 
  FixNumFmt = HTMLFormatting.Dynamic OBJECT
    prec : CARDINAL := 3;
    style := Fmt.Style.Fix;
  OVERRIDES
    format := FNFF;
  END;

PROCEDURE FNFF(fnf : FixNumFmt; what : HTML.Stuff) : HTML.Stuff =
  BEGIN
    TYPECASE what OF 
      TEXT(t) => 
        TRY
          WITH num = Scan.LongReal(t) DO
            RETURN Fmt.LongReal(num, style := fnf.style, prec := fnf.prec)
          END
        EXCEPT
          Lex.Error, FloatMode.Trap => RETURN what
        END
    ELSE
      RETURN what
    END
  END FNFF;

PROCEDURE MakeAlignments(READONLY cols : ARRAY OF TEXT) : Alignment =
  VAR 
    res : Alignment := NIL;
  BEGIN
    FOR i := FIRST(cols) TO LAST(cols) DO
      res := NEW(Alignment, column := cols[i], next := res)
    END;
    RETURN res
  END MakeAlignments;

PROCEDURE CheckSignin(p : Page; request : Request.T)  RAISES { DBerr.Error }  = <* FATAL FloatMode.Trap, Lex.Error *>
  VAR
    login, password : TEXT;
    result : Database.Table;
    uid := Scan.Int(result.getUniqueEntry("uid"));
    remoteAddr : TEXT;
  BEGIN
    IF NOT request.getEnvVar("REMOTE_ADDR", remoteAddr) THEN
      HTML.Error("REMOTE_ADDR not defined in environment!");
    END;
    request.session := NEW(Session.T).init(1, remoteAddr);
    EVAL p.page.setBody("Welcome to the GC website, " &
      result.getUniqueEntry("name") & "<br>\n Your session ID will be " &
      request.session.getId() & "<br>\n");
    
    Debug.Out("Building initial page contents...");
    WITH link = NEW(HTMLLink.T).init("Continue.","holding_exec",request) DO
      p.page.addToBody(link);
      
      (* make it redirect automatically *)
      EVAL p.page.setHead("<META HTTP-EQUIV=\"Refresh\" CONTENT=\"1; URL=" &
        link.URL() & "\"")
    END;
    
    Debug.Out("Done.");
  END CheckSignin;

TYPE 
  Page = PageDispatch.T OBJECT 
    page : HTMLPage.T;
    title : TEXT := NIL;
    headerStuff : TEXT :="";
  METHODS
    body(request : Request.T) RAISES { DBerr.Error } ;
  OVERRIDES
    display := GCDisplay;
  END;

  RedirPage = PageDispatch.T OBJECT
    target : TEXT;
  OVERRIDES
    display := RDisplay;
  END;

  GCPage = HTMLPage.T OBJECT
    p : Page;
  OVERRIDES
    renderHeader := GCRenderHeader;
  END;

CONST
  TinyTextHeader = "<LINK rel=\"stylesheet\" href=\"tiny.css\" type=\"text/css\">";

  
PROCEDURE GCRenderHeader(p : GCPage) : TEXT =
  VAR
    more := "";
  BEGIN 
    IF p.p # NIL THEN
      more := p.p.headerStuff
    END;
    RETURN HTMLPage.T.renderHeader(p) & more
  END GCRenderHeader;

PROCEDURE RDisplay(p : RedirPage; req : Request.T) : HTMLPage.T =
  BEGIN
    WITH res = NEW(GCPage, p := NIL),
         link = NEW(HTMLLink.T).init("Continue.",p.target, req) 
     DO
      EVAL res.setHead("<META HTTP-EQUIV=\"Refresh\" CONTENT=\"0; URL="&
        link.URL() & "\"");
      res.addToBody("<h1>Redirecting, please wait...</h1>");
      RETURN res
    END
  END RDisplay;

PROCEDURE GCDisplay(p : Page; req : Request.T) : HTMLPage.T
  RAISES { DBerr.Error } =
  BEGIN 
    p.page := NEW(GCPage, p := p);
    IF p.title # NIL THEN
      p.page.addToBody("<p><h1>"&p.title&"</h1><p>\n")
    ELSE
      p.page.addToBody("<p><h1>Page \"" & req.toPage & "\"</h1><p>\n")
    END;
    p.body(req);
    RETURN p.page
  END GCDisplay;

PROCEDURE InitDB() = 
  BEGIN now := Time.Now() END InitDB;

VAR
  now     : Time.T;
  dbName  : TEXT;
  CGIname := Pathname.Last(Params.Get(0));
BEGIN
  WITH mainPage = NEW(Page, body := Main, title := "Navigation Page")  DO
    Pages.AddDispatch ( "main", mainPage, signinNeeded := FALSE );
    Pages.AddDispatch ( "",     mainPage, signinNeeded := FALSE)
  END;

  Pages.AddDispatch ( "signin",
                      NEW(Page, body := Signin ),
                      signinNeeded := FALSE);
  
  Pages.AddDispatch ( "check_signin",
                      NEW(Page, body := CheckSignin ),
                      signinNeeded := FALSE );

  Pages.AddDispatch ( "run_sql",
                      NEW(Page,
                          body := RunSQL,
                          privLevel := Pages.Priv.Admin,
                          title := "Run SQL Query" ) );
  
END Chipdev.
