(* $Id$ *)

MODULE Chipdev;
IMPORT DBerr;
IMPORT HTML, HTMLPage, HTMLForm, HTMLInput, HTMLLink, HTMLList;
IMPORT HTMLTextArea;
IMPORT Pages,Debug,Database, DatabaseUtils, Text, Session, Scan;
IMPORT Fmt; FROM Fmt IMPORT Bool, LongReal, F, Int;
IMPORT Lex, FloatMode;
IMPORT Request;
IMPORT TextReader;
IMPORT PageDispatch;
IMPORT HTMLFormatting; FROM HTMLFormatting IMPORT Alignment, RowFormat;
IMPORT Process;
IMPORT HTMLTable;
IMPORT Thread;
IMPORT XTime AS Time;
IMPORT Pathname, Params;
IMPORT FS, OSError;
IMPORT SysPerf;
IMPORT TextTable;
IMPORT DatabaseClass; (* to get directly at DatabaseTable.T.data *)

CONST TE = Text.Equal;

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

PROCEDURE SiteNavBar(request : Request.T) : HTMLList.T =

  PROCEDURE AddLinkTo(named   : TEXT;
                      titleP  : TEXT := NIL;
                      getVars : TextTable.T := NIL) =
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
        res.add(NEW(HTMLLink.T).init(title, named, request, getVars := getVars))
      END
    END AddLinkTo;

  VAR
    res     := NEW(HTMLList.T);
    spacing := "&nbsp;&nbsp;&nbsp;";
  BEGIN
    Debug.Out("Building SiteNavBar...");

    AddLinkTo("holding_exec");
    res.add(spacing);

(*
    AddLinkTo("do_exec");
    res.add(spacing);
*)

    AddLinkTo("regression");
    res.add(spacing);

    AddLinkTo("state_rejected");
    res.add(spacing);

    AddLinkTo("do_clear");
    res.add(spacing);

    res.add("<br>");

    Debug.Out(F("Chipdev.SiteNavBar: (getVars=NIL)=%s",
              Bool(request.getGetVars()=NIL)), 0);
    AddLinkTo("main", getVars := request.getGetVars());
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


PROCEDURE Regression(p : Page; request : Request.T) =

  PROCEDURE MakeButton(q : [1..LAST(CARDINAL)]) =
    BEGIN
      Debug.Out(F("MakeButton(%s)", Int(q)));
    END MakeButton;
    
  VAR
    rowsPerPage := 25;
    maxid := -1;
    restriction := "";
    page : [ 1..LAST(CARDINAL) ] := 1; (* me *)
    v : TEXT;
    totalRows : CARDINAL;
    npages : CARDINAL;
  BEGIN
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");

    (* do the pagination calcs *)

    (* first, my page? *)
    IF request.getGetVars().get("page", v) THEN
      page := Scan.Int(v)
    END;

    Debug.Out("My page is " & Int(page));

    WITH query = "select count(*) as count from verification_regression",
         res   = Database.TExec(query) DO
      totalRows := Scan.Int(res.getUniqueEntry("count"))
    END;

    npages := (totalRows - 1) DIV rowsPerPage + 1;

    VAR
      q : INTEGER := page;
      step : CARDINAL := 1;
    BEGIN
      WHILE q # 1 DO
        q := q - step;
        q := MAX(q, 1);

        MakeButton(q);

        step := step * 2
      END
    END;
    
    VAR
      q := page;
      step := 1;
    BEGIN
      WHILE q # npages DO
        q := q + step;
        q := MIN(q, npages);

        MakeButton(q);

        step := step * 2
      END
    END;

    WITH offset = page * rowsPerPage, (* which row to start at *)
         query = F("select r.id, r.name, r.username, r.date, r.status, r.comment, (select count(*) from verification_testrun t0 where t0.regression_id=r.id and t0.status='pass') as 'p', (select count(*) from verification_testrun t1 where t1.regression_id=r.id and t1.status='fail') as 'f',  (select count(*) from verification_testrun t2 where t2.regression_id=r.id) as 'total' from verification_regression r %s order by r.id desc limit %s offset %s",
                   restriction,
                   Int(rowsPerPage),
                   Int(offset)),

         res   = Database.TExec(query),
         arr   = NEW(REF ARRAY OF ARRAY OF HTML.T,
                     res.getNumRows(), res.getNumCols()) DO
      FOR r := FIRST(arr^) TO LAST(arr^) DO
        FOR c := FIRST(arr[0]) TO LAST(arr[0]) DO
          arr[r,c] := HTMLList.Hack(res.data[r,c])
        END
      END;

      WITH res = NEW(HTMLTable.T).init(arr) DO
        p.page.addToBody(res)
      END
    END

  END Regression;
  
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
            RETURN LongReal(num, style := fnf.style, prec := fnf.prec)
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

PROCEDURE CheckSignin(p : Page; request : Request.T)
  RAISES { DBerr.Error }  = <* FATAL FloatMode.Trap, Lex.Error *>
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

  Pages.AddDispatch ( "regression",
                      NEW(Page, body := Regression ),
                      signinNeeded := FALSE);
  
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
