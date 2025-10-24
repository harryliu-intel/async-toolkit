(* $Id$ *)

MODULE GCSite;
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
IMPORT TradingLedger, Ledger;
IMPORT XTime AS Time;
IMPORT Pathname, Params;
IMPORT Random, FileWr, FS, OSError;
FROM TradingLedgerUtils2 IMPORT GetTrades, Restriction, TradeBalanceTab, 
                                BalanceTabBalance, IsClosingTrade, 
                                StateExecutedQuery, ForceExit,
                                CumulativePLQuery;
IMPORT TZ;
IMPORT PriceWatch;
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

PROCEDURE SigninURL() : TEXT = 
  BEGIN 
    RETURN "<a href=\"" & CGIname & "?signin\">Signin page.</a>" 
  END SigninURL;

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
        delta := P("day",d DIV 86400); d := d MOD 86400
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

PROCEDURE FindFakeTime() : Time.T =
  BEGIN
    TRY
      VAR
        now := Ledger.NowIsNow;
        val := Database.TExec("select value from config where var='fake_time';").getUniqueEntry("value");
      BEGIN
        EVAL Database.TExec("set time zone 'America/New_York';");

        IF NOT TE(val,"") THEN
          now := Database.TExec(
  "select extract('epoch' from ('"&val&"'::timestamp with time zone));"
          ).getLRI(0)
        END;
        RETURN now
      END
    EXCEPT
      FloatMode.Trap, Lex.Error =>
        HTML.Error("FindFakeTime: couldn't parse LongReal")
    |
      DBerr.Error(s) => 
        HTML.Error("FindFakeTime: DBerr.Error raised: \"" & s & "\"")
    END;
    <* ASSERT FALSE *>
  END FindFakeTime;

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

PROCEDURE ListUsers(p : Page; request : Request.T)  
  RAISES { DBerr.Error } =
  VAR
    userList := Database.TExec("SELECT u.uid, u.organization AS org, o.name AS \"organization name\", o.country, u.title, u.name, u.email, u.status FROM userinfo u, organization_tbl o WHERE o.orgid = u.organization");
  BEGIN
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
    p.page.addToBody(userList.allColsToHTML());
  END ListUsers;

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

(* run any SQL query, privleges req'd *)
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

PROCEDURE SelType(type : TEXT) : HTMLTable.T RAISES { DBerr.Error } = 
  BEGIN
    RETURN Database.TExec(
  "select acctno,name,flt8_mul_cash(balance,'$1') as balance from ledger"&
  " where type='"&type&"' "&
  "union "&
  "select null,'<STRONG>TOTAL "&type&"</STRONG>',"&
  "       flt8_mul_cash(sum(balance),'$1') from ledger where type='"&type&"';").allColsToHTML(NEW(Alignment, column := "balance"))
  END SelType;

PROCEDURE BSheetTab() : HTMLTable.T RAISES { DBerr.Error } =
  VAR
    ass := SelType("asset");
    lia := SelType("liability");
    equ := SelType("equity");
  BEGIN
    RETURN HTMLTable.Horiz(ass,
                           HTMLTable.Vert(lia,equ),
                           formatting := NEW(Alignment, column := "1"))
  END BSheetTab;

PROCEDURE BalanceSheet(p : Page; request : Request.T)  RAISES { DBerr.Error } =
  BEGIN
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
    p.page.addToBody(BSheetTab());
  END BalanceSheet;

PROCEDURE Holdings(p : Page; request : Request.T)  RAISES { DBerr.Error } =
  VAR
    journal := Database.TExec(
  "select serial,date_trunc('second',at) as at ,description,flt8_mul_cash(amount,'$1') as amount,l.name as debit,m.name as credit,trade "&
  "  from journal j, ledger l, ledger m "&
  "  where j.debit_acct=l.acctno and j.credit_acct=m.acctno order by -serial;"
    );

  BEGIN
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
    p.page.addToBody("<h3>JOURNAL:</h3><br>\n");
    p.page.addToBody(journal.allColsToHTML(MakeAlignments(ARRAY OF TEXT { "amount", "serial", "trade" })))
  END Holdings;


PROCEDURE AddSound(p : Page) =
  CONST
    fn = "4bells.wav";
  BEGIN
    p.page.addScript(
"<SCRIPT TYPE=\"text/javascript\">\n"&
" <!-- \n"&
"var filename=\""&fn&"\";\n"&
"if (navigator.appName == \"Microsoft Internet Explorer\")\n"&
"    document.writeln ('<BGSOUND SRC=\"' + filename + '\">');\n"&
"else if (navigator.appName == \"Netscape\")\n"&
"    document.writeln ('<EMBED SRC=\"' + filename + '\" AUTOSTART=TRUE WIDTH=144 HEIGHT=60><P>');\n"&
"// -->\n"&
"</SCRIPT>\n"&
"<NOSCRIPT>\n"&
"<BGSOUND SRC=\""&fn&"\">\n"&
"</NOSCRIPT>\n"
          )
  END AddSound;

PROCEDURE HoldingExec(p : Page; request : Request.T)  RAISES { DBerr.Error } =
  CONST
    hlSize = "H3";
  VAR
    ids : Database.Vector;
    update : TEXT;
    bigTable := NEW(REF ARRAY OF ARRAY OF HTML.T, 1, 2);
  BEGIN
    IF request.getPostVar("update", update) THEN
      TRY
        VAR
          reader := NEW(TextReader.T).init(update);
          w1, w2 := reader.nextE(" ");
          trading_id := Scan.Int(w2);
          ok := FALSE;
          ledger : TradingLedger.T := NEW(TradingLedger.T).initFromDB(Database.Static()); 
        BEGIN
          ledger.begin();
          TRY
            IF    TE(w1, "ACCEPT") THEN
              ledger.pend(trading_id,now)
            ELSIF TE(w1, "REJECT") THEN 
              ledger.reject(trading_id,now)
            ELSIF TE(w1, "ACKNOWLEDGE_CANCEL") THEN 
              ledger.ackCancel(trading_id,now)
            ELSIF TE(w1, "TRADE_B") THEN
              DoExecB(p,request); RETURN
            ELSIF TE(w1, "FINALIZE") THEN 
              VAR
                closingSerial : CARDINAL;
              BEGIN
                IF IsClosingTrade(trading_id,closingSerial) THEN
                  ledger.finalizeClosing(trading_id,closingSerial,now)
                ELSE
                  ledger.finalizeOpening(trading_id,now)
                END
              END
              
            ELSIF TE(w1, "REVERSE") THEN 
              HTML.Error("REVERSE not yet supported.  "&
                "Instead finalize and force exit at purchase price.",
                doQuit := TRUE);

            ELSE
              ledger.abort();
              HTML.Error("Unknown action in HoldingExec: request is \"" & 
                update & "\"", doQuit := TRUE);
            END;
            (* we don't get here if we had an exception *)
            ok := TRUE;
          FINALLY
            IF ok THEN 
              ledger.commit()  (* normal exit *)
            ELSE 
              ledger.abort()   (* exception exit *)
            END
          END
        END
      EXCEPT
        TextReader.NoMore => 
          HTML.Error("TextReader.NoMore exception in HoldingExec while parsing \"" & update & "\"")
      |
        Lex.Error, FloatMode.Trap =>
          HTML.Error("Trouble reading number in HoldingExec while parsing \"" & update & "\"")
      END;

      (* redirect back *)
      p.page := NEW(RedirPage, target := "redirect_holding_exec").display(request);
      RETURN
    END;

    p.page.addJavaScriptRefresh(RefreshInterval);
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");

    ids := Database.TExec(
  "select distinct trading_id from trading_details where state='holding';"
    ).getColByName("trading_id");

    WITH l1 = NEW(HTMLList.T) DO
      l1.add("<"&hlSize&">HOLDING FOR APPROVAL---</"&hlSize&">\n");

      WITH tab = GetTrades(Restriction.Holding,
                           trade := FALSE, stateAt := TRUE),
           html = tab.allColsToHTML(TradesFormatting(request), RowFormatting()) DO

        IF tab.rows() > 0 THEN
          (* add sound *)
          AddSound(p)
        END;
        l1.add(html);
      END;

      l1.add("<"&hlSize&">---END</"&hlSize&">\n");
      l1.add(HTMLTable.Horiz(
                 ActionForm("ACCEPT ALL HOLDING",request),
                 ActionForm("REJECT ALL HOLDING",request)));

      bigTable[0,0] := l1;

    END;

    WITH l2 = NEW(HTMLList.T) DO
      FillInExecPage(l2,request);
      l2.add(HTMLTable.Horiz(
                 ActionForm("ACK_CANCEL ALL CANCELING",request),
                 ActionForm("REJECT ALL PENDING",request)));

      bigTable[0,1] := l2
    END;

    p.page.addToBody(NEW(HTMLTable.T).init(bigTable,
              rowFormatting := HTMLFormatting.MakeRowFormat(valign:="top")));

    TRY
      p.page.addToBody(SiteStatus())
    EXCEPT
      FormatError=>
        p.page.addToBody("<br><br> ERROR ATTEMPTING TO MONITOR FEEDS (FormatError).  PLEASE NOTIFY ADMINISTRATOR.<br>")
    |
      OSError.E(al) =>
        p.page.addToBody("<br><br> ERROR ATTEMPTING TO MONITOR FEEDS (OSError.E:"&AL.Format(al)&" ).  PLEASE NOTIFY ADMINISTRATOR.<br>")
    END;

    p.page.addToBody("</font>");

  END HoldingExec;

PROCEDURE StateExecuted(p : Page; request : Request.T)  RAISES { DBerr.Error } =
  VAR
    data := StateExecutedQuery();
    update : TEXT;
  BEGIN
    IF request.getPostVar("update", update) THEN
      TRY
        VAR
          reader := NEW(TextReader.T).init(update);
          w1, w2, w3 := reader.nextE(" ");
          closingSerial := Scan.Int(w3);
        BEGIN
          IF TE(w1, "FORCE") AND TE(w2, "EXIT") THEN

            ForceExit(closingSerial);
          ELSE
            HTML.Error("Unknown action in StateExecuted: request is \"" & 
              update & "\"", doQuit := TRUE);
          END
        END
      EXCEPT
        TextReader.NoMore => 
          HTML.Error("TextReader.NoMore exception in StateExecuted while parsing \"" & update & "\"")
      |
        Lex.Error, FloatMode.Trap =>
          HTML.Error("Trouble reading number in StateExecuted while parsing \"" & update & "\"")
      END;

      p.page := NEW(RedirPage, 
                    target := "redirect_state_executed").display(request);
      RETURN
    END;

    p.page.addJavaScriptRefresh(RefreshInterval);
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");

    p.page.addToBody(data.allColsToHTML(TradesFormatting(request,
                         targetPage := request.toPage),RowFormatting()));

    p.page.addToBody(ActionForm("FORCE EXIT ALL", request))
  END StateExecuted;

PROCEDURE PLLog(p : Page; request : Request.T)  RAISES { DBerr.Error } =
  VAR
    data1 := CumulativePLQuery(active := FALSE);
    data2 := CumulativePLQuery(active := TRUE);
  BEGIN
    p.page.addJavaScriptRefresh(RefreshInterval);
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");

    p.page.addToBody("<H3>CLOSED TRADES</H3>\n");
    p.page.addToBody(data1.allColsToHTML(TradesFormatting(request,
                         targetPage := request.toPage),RowFormatting()));

    TRY
      WITH data3 = CumulativePLQuery(active := FALSE, forGraphing := TRUE),
           opens = CumulativePLQuery(active := TRUE, forGraphing := TRUE),
           rn = Fmt.Int(NEW(Random.Default).init().integer(1,LAST(CARDINAL)),
                        base := 16),
           fnC = TmpDir & "/GCSiteC." & rn, 
           fnO = TmpDir & "/GCSiteO." & rn, 
                                              
           pngFn = dbName & "_pl.png",
           wrC = FileWr.Open(fnC),
           wrO = FileWr.Open(fnO),
           cfn = TmpDir & "/GCSite." & rn & ".cmds",
           gwr = FileWr.Open(cfn) DO
        TRY
          FOR i := 0 TO data3.rows()-1 DO
            WITH r = data3.getRow(i),
                 startTime = r.getI(1), endTime = r.getI(2), PL = r.getI(3) DO
              IF NOT TE(PL,"") THEN
                Wr.PutText(wrC, startTime);
                Wr.PutChar(wrC, ' ');
                Wr.PutText(wrC, "0\n");
                Wr.PutText(wrC, endTime);
                Wr.PutChar(wrC, ' ');
                Wr.PutText(wrC, PL);
                Wr.PutChar(wrC, '\n'); Wr.PutChar(wrC, '\n')
              END
            END
          END;

          FOR i := 0 TO opens.rows()-1 DO
            WITH r = opens.getRow(i),
                 startTime = r.getI(1), PL = r.getI(3) DO
              IF NOT TE(PL,"") THEN
                Wr.PutText(wrO, startTime);
                Wr.PutChar(wrO, ' ');
                Wr.PutText(wrO, "0\n");
                Wr.PutText(wrO, Fmt.LongReal(Time.Now()));
                Wr.PutChar(wrO, ' ');
                Wr.PutText(wrO, PL);
                Wr.PutChar(wrO, '\n'); Wr.PutChar(wrO, '\n')
              END
            END
          END;

          Wr.PutText(gwr,"set data style lines\nset term png size 1024,768\n"&
            "set output \"/home/mika/public_html/cgi-bin/wwwscratch/"&pngFn&"\"\n");
          IF opens.rows()>0 THEN
            Wr.PutText(gwr,"plot 0,\""&fnC&"\" title \"closed\" ,"&
              "\""&fnO&"\" title \"open\"\n")
          ELSE
            Wr.PutText(gwr,"plot 0,\""&fnC&"\" title \"closed\"\n")
          END;
          
          Wr.PutText(gwr,"quit\n");
          
          Wr.Close(wrC);  Wr.Close(wrO);  Wr.Close(gwr);
          
          WITH proc = Process.Create(GnuplotPath, ARRAY OF TEXT{cfn}) DO
            EVAL Process.Wait(proc)
          END;

          p.page.addToBody("<br><br><img src=\"wwwscratch/"&pngFn&"\">")

        FINALLY
          IF DoDeleteGnuplotFiles THEN
            FS.DeleteFile(fnC); FS.DeleteFile(fnO); FS.DeleteFile(cfn)
          END
        END
      END;

    EXCEPT
      OSError.E, Wr.Failure => 
        HTML.Error("PLLog: trouble generating graph!")
    END;

    p.page.addToBody("<br><br>");
    p.page.addToBody("<H3>OPEN TRADES</H3>\n");
    p.page.addToBody(data2.allColsToHTML(TradesFormatting(request,
                         targetPage := request.toPage),RowFormatting()));

  END PLLog;

PROCEDURE MatchingButton(b : Button; t : TEXT; VAR rest : TEXT) : BOOLEAN =
  VAR
    len := Text.Length(BNames[b]);
  BEGIN
    IF TE(BNames[b],Text.Sub(t,0,len)) THEN
      rest := Text.Sub(t,len);
      RETURN TRUE
    END;
    RETURN FALSE
  END MatchingButton;

PROCEDURE PerformAction(p : Page; request : Request.T) RAISES { DBerr.Error } =
  <*FATAL FloatMode.Trap, Lex.Error *>
  VAR
    button, actionTarget : TEXT;
    q := NEW(REF TEXT);
    ledger : TradingLedger.T := NEW(TradingLedger.T).initFromDB(Database.Static()); 
  BEGIN
    IF NOT request.getPostVar("action_target", actionTarget) OR
       NOT request.getPostVar("update", button) THEN
      HTML.Error("PerformAction: need both action_target and update")
    END;

    IF TE(button, "SET ALL HISTORICAL") THEN
      WITH col = GetTrades(Restriction.ReadyToGoHistorical,
                           trade := FALSE,
                           debugQuery:=q).getColByName("basket_id")^,
           done = NEW(CardSetDef.T).init() DO
        FOR i := FIRST(col) TO LAST(col) DO
          WITH id = Scan.Int(col[i]) DO
            IF NOT done.insert(id) THEN
              ledger.expire(id,now)
            END
          END
        END
      END
    ELSIF TE(button, "FORCE EXIT ALL") THEN
      WITH col = StateExecutedQuery().getColByName("basket_id")^,
           done = NEW(CardSetDef.T).init() DO
        FOR i := FIRST(col) TO LAST(col) DO
          WITH id = Scan.Int(col[i]) DO
            IF NOT done.insert(id) THEN
              ForceExit(id)
            END
          END
        END
      END
    ELSIF TE(button, "ACCEPT ALL HOLDING") THEN
      WITH col = GetTrades(Restriction.HoldingCanPend,
                           trade := FALSE).getColByName("basket_id")^,
           done = NEW(CardSetDef.T).init() DO
        FOR i := FIRST(col) TO LAST(col) DO
          WITH id = Scan.Int(col[i]) DO
            IF NOT done.insert(id) THEN
              ledger.pend(id,now)
            END
          END
        END
      END
    ELSIF TE(button, "REJECT ALL HOLDING") THEN
      WITH col = GetTrades(Restriction.Holding,
                           trade := FALSE).getColByName("basket_id")^,
           done = NEW(CardSetDef.T).init() DO
        FOR i := FIRST(col) TO LAST(col) DO
          WITH id = Scan.Int(col[i]) DO
            IF NOT done.insert(id) THEN
              ledger.reject(id,now)
            END
          END
        END
      END    
    ELSIF TE(button, "REJECT ALL PENDING") THEN
      WITH col = GetTrades(Restriction.Pending,
                           trade := FALSE).getColByName("basket_id")^,
           done = NEW(CardSetDef.T).init() DO
        FOR i := FIRST(col) TO LAST(col) DO
          WITH id = Scan.Int(col[i]) DO
            IF NOT done.insert(id) THEN
              ledger.reject(id,now)
            END
          END
        END
      END
    ELSIF TE(button, "ACK_CANCEL ALL CANCELING") THEN
      WITH col = GetTrades(Restriction.Canceling,
                           trade := FALSE).getColByName("basket_id")^,
           done = NEW(CardSetDef.T).init() DO
        FOR i := FIRST(col) TO LAST(col) DO
          WITH id = Scan.Int(col[i]) DO
            IF NOT done.insert(id) THEN
              ledger.ackCancel(id,now)
            END
          END
        END
      END
    ELSE
      HTML.Error("PerformAction: Unknown action button \""  & button & "\"",
                 doQuit := TRUE)
    END;

    p.page := NEW(RedirPage, target := actionTarget).display(request)
  END PerformAction;

PROCEDURE DoClear(p : Page; request : Request.T) RAISES { DBerr.Error } =
  VAR
    q := NEW(REF TEXT);
    update, w3 : TEXT;
    ledger : TradingLedger.T := NEW(TradingLedger.T).initFromDB(Database.Static()); 
  BEGIN
    IF request.getPostVar("update", update) THEN
      TRY
        IF MatchingButton(B.MoveHistorical,update,w3) THEN
          WITH basket = Scan.Int(w3) DO
            ledger.expire(basket,now)
          END
        END
      EXCEPT
        Lex.Error, FloatMode.Trap =>
          HTML.Error("Trouble reading number in DoClear while parsing \"" & update & "\"")
      END;
      p.page := NEW(RedirPage, 
                    target := "redirect_do_clear").display(request);
      RETURN
    END;

    p.page.addJavaScriptRefresh(RefreshInterval);
    WITH data = GetTrades(Restriction.ReadyToGoHistorical,
                          trade := FALSE,
                          debugQuery:=q).allColsToHTML(TradesFormatting(request,
                                                                                                                                                    targetPage := request.toPage),RowFormatting()) DO
      p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
      IF PrintSQL THEN
        p.page.addToBody(q^)
      END;
      p.page.addToBody("<br><br>");
      p.page.addToBody(data);
      p.page.addToBody("<br>");
      p.page.addToBody(ActionForm("SET ALL HISTORICAL", request))
    END
  END DoClear;

PROCEDURE ActionForm(action : TEXT; request : Request.T) : HTMLForm.T =
  BEGIN
    WITH form = NEW(HTMLForm.T).init("perform_action",request) DO
      form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                     value := request.toPage,
                                     name := "action_target"));
      form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                     value := action,
                                     name := "update"));
      RETURN form
    END
  END ActionForm;

PROCEDURE StateRejected(p : Page; request : Request.T)  RAISES { DBerr.Error } =
  VAR
    ids := Database.TExec(
  "select distinct trading_id from trading_details where state='rejected' order by trading_id;"
    ).getColByName("trading_id");
    update : TEXT;
  BEGIN
    (* update comments first *)
    FOR i := FIRST(ids^) TO LAST(ids^) DO
      IF request.getPostVar("do_update_reason:"&ids[i],update) THEN
        IF request.getPostVar("update_reason:"&ids[i],update) THEN
          EVAL Database.TExec(
  "update state_changes set comment='"&Database.Escape(update)&"' from trading_details td where state_changes.td_serial=td.serial and trading_id="&ids[i]&";"
          )
        END
      END
    END;
    
    p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");

    FOR i := FIRST(ids^) TO LAST(ids^) DO
      TRY
      WITH id = Scan.Int(ids[i]),
           data = Database.TExec(
  "select t.trading_id as basket_id, t.serial,t.type,t.ticker, t.state,  date_trunc('second',s.at), s.comment as reason from trading_details t, state_changes s where t.last_state_change=s.serial and t.state='rejected' and t.trading_id="&Fmt.Int(id)&";"
        ),
           form = NEW(HTMLForm.T).init("state_rejected",request) DO
        
        p.page.addToBody(data.allColsToHTML(TradesFormatting(request)));
        p.page.addToBody("<br>");

        form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text,
                                       name := "update_reason:"&Fmt.Int(id)));
        form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                       value := "Change reason",
                                       name := "do_update_reason:"&Fmt.Int(id)));

        p.page.addToBody(form);
        p.page.addToBody("<br>");
        
      END
      EXCEPT
        FloatMode.Trap, Lex.Error =>
          HTML.Error("StateRejected unable to parse \"" & ids[i] & "\" as number.")
      END
    END;

  END StateRejected;

PROCEDURE DoExec(p : Page; request :Request.T) RAISES { DBerr.Error } =
  VAR
    exec : TEXT;
  BEGIN
    IF request.getPostVar("exec",exec) THEN
      TRY
        VAR
          reader := NEW(TextReader.T).init(exec);
<*NOWARN*>w1, w2 := reader.nextE(" ");
          trade_id := Scan.Int(w2);
        BEGIN
          WITH tab = Database.TExec(
  "select trading_id as basket_id, serial, type, ticker, state, count, (select sum(e.count+0) from executions e where e.td_serial=trading_details.serial) as so_far, target_price, strategy from trading_details where serial=" & Fmt.Int(trade_id) & ";"
            ), format = TradesFormatting(request),
               ticker = tab.getUniqueEntry("ticker"),
               toGo = tab.getIntOrZero("count")-tab.getIntOrZero("so_far") DO
            EVAL p.page.setHead(
  "<script type=\"text/javascript\">\n" &
  "function numbersonly(e)\n" &
  "{\n" &
  "if (e.keyCode!=8 && e.keyCode!=9 && e.charCode!=45 && e.charCode!=46 &&\n" &
  "     (e.charCode < 48 || e.charCode > 57)) \n" &
  "   return false;\n" &
  "return true;\n" &
  "}\n" &
  "</script>"
            );

            p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
            p.page.addToBody(tab.allColsToHTML(format));
            p.page.addToBody("<br><br><STRONG>EXECUTED</STRONG>\n");
            p.page.addToBody("<br>");
            WITH form = NEW(HTMLForm.T).init("confirm_trade",request) DO
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                             name := "num_trades",
                                             value := "1"));

              (****************************************)
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "shares0",
                                             value := Fmt.Int(toGo),
                                             onkeypress:="return numbersonly(event)",
                                             onkeyup:="commission0.value=Math.abs(shares0.value)*0.005"));
              form.add(" shares ");
              form.add("<strong>" & ticker & "</strong>");
              form.add(" @ $");
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "price0"));
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                             name := "ticker0",
                                             value := ticker));
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                             name := "trade_id0",
                                             value := Fmt.Int(trade_id)));

              form.add("w/ commissions:");
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "commission0",
                         value := Fmt.LongReal(0.005d0*ABS(FLOAT(toGo,LONGREAL)))));
              form.add("comments:");
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "comment0"));
              (****************************************)

              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                             name := "record_execution", 
                                             value := "RECORD TRADE"));

              p.page.addToBody(form)
            END;
            p.page.addToBody("<br><br>");
            WITH tradeTab = Database.TExec(
  "select serial as exec_id, at as entered_at, td_serial as serial, ticker, count, flt8_mul_cash(price,'$1') as price, flt8_mul_cash(commissions,'$1') as commissions, comment"&
  "  from executions where td_serial="&Fmt.Int(trade_id) & ";"
              ),
                 tickerTab = Database.TExec(
  "select t.trading_id as basket_id, t.serial, t.type, t.ticker, t.state, t.count, t.target_price, flt8_mul_cash(t.bookcost,'$1') as bookcost, t.strategy from trading_details t, trading_details u where t.ticker=u.ticker and u.serial=" & Fmt.Int(trade_id) & " and (t.state != 'rejected' and t.state != 'historical' and t.state != 'canceled') order by serial;"
              ),
                 tickerTabHist = Database.TExec(
  "select t.trading_id as basket_id, t.serial, t.type, t.ticker, t.state, t.count, t.target_price, flt8_mul_cash(t.bookcost,'$1') as bookcost, t.strategy from trading_details t, trading_details u where t.ticker=u.ticker and u.serial=" & Fmt.Int(trade_id) & " and (t.state = 'rejected' or t.state='historical' or t.state = 'canceled') order by serial;"
              ),
                 basketTab = Database.TExec(
  "select t.trading_id as basket_id, t.serial, t.type, t.ticker, t.state, t.count, t.target_price, flt8_mul_cash(t.bookcost,'$1') as bookcost, t.strategy from trading_details t, trading_details u where t.trading_id=u.trading_id and u.serial=" & Fmt.Int(trade_id) & " order by serial;"
              ) DO
              p.page.addToBody("<hr>\n");
              p.page.addToBody("<h3>PARTIAL EXECUTIONS SO FAR:</h3><br>\n");
              p.page.addToBody(tradeTab.allColsToHTML(format));
              p.page.addToBody("<h3>PENDING/URGENT TRADES WITH THIS TICKER:</h3><br>\n");
              p.page.addToBody(tickerTab.allColsToHTML(format));
              p.page.addToBody("<h3>HISTORICAL TRADES WITH THIS TICKER:</h3><br>\n");
              p.page.addToBody(tickerTabHist.allColsToHTML(format));
              p.page.addToBody("<br><h3>BASKET TRADES:</h3><br>\n");
              p.page.addToBody(basketTab.allColsToHTML(format));
            END
             
            
          END
        END
      EXCEPT
        TextReader.NoMore =>
          HTML.Error("TextReader.NoMore exception in DoExec while parsing \"" & exec & "\"")
      |
        Lex.Error, FloatMode.Trap => 
          HTML.Error("Trouble reading number in DoExec while parsing \"" & exec & "\"")
      END
    ELSE
      HTML.Error("Got to do_exec without an exec??? Check your URL!", doQuit := TRUE)
    END;

  END DoExec;

PROCEDURE DoExecB(p : Page; request :Request.T) RAISES { DBerr.Error } =
  VAR
    exec : TEXT;
  BEGIN
    p.headerStuff := "";
    IF request.getPostVar("update",exec) THEN
      TRY
        VAR
          reader := NEW(TextReader.T).init(exec);
<*NOWARN*>w1, w2 := reader.nextE(" ");
          basket_id := Scan.Int(w2);
        BEGIN
          WITH tab = Database.TExec(
  "select trading_id as basket_id, serial, type, ticker, state, count, (select sum(e.count+0) from executions e where e.td_serial=trading_details.serial) as so_far, target_price, strategy from trading_details where trading_id=" & Fmt.Int(basket_id) & ";"
            ), format = TradesFormatting(request) DO

            EVAL p.page.setHead(
  "<script type=\"text/javascript\">\n" &
  "function numbersonly(e)\n" &
  "{\n" &
  "if (e.keyCode!=8 && e.keyCode!=9 && e.charCode!=45 && e.charCode!=46 &&\n" &
  "     (e.charCode < 48 || e.charCode > 57)) \n" &
  "   return false;\n" &
  "return true;\n" &
  "}\n" &
  "</script>"
            );

            p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
            p.page.addToBody(tab.allColsToHTML(format));
            p.page.addToBody("<br><br><STRONG>EXECUTED</STRONG>\n");
            p.page.addToBody("<br>");
            WITH form = NEW(HTMLForm.T).init("confirm_trade",request) DO
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                             name := "num_trades",
                                             value := Fmt.Int(tab.rows())));

              FOR r := 0 TO tab.rows()-1 DO
            WITH
              row = tab.getRow(r),
              rT = Fmt.Int(r),
              trade_id = row.getInt("serial"),
               ticker = row.getUniqueEntry("ticker"),
               toGo = row.getIntOrZero("count")-row.getIntOrZero("so_far") DO
              (****************************************)
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "shares"&rT,
                                             value := Fmt.Int(toGo),
                                             onkeypress:="return numbersonly(event)",
                                             onkeyup:="commission"&rT&".value=Math.abs(shares"&rT&".value)*0.005"));
              form.add(" shares ");
              form.add("<strong>" & ticker & "</strong>");
              form.add(" @ $");
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "price"&rT));
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                             name := "ticker"&rT,
                                             value := ticker));
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                             name := "trade_id"&rT,
                                             value := Fmt.Int(trade_id)));

              form.add("w/ commissions:");
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "commission"&rT,
                         value := Fmt.LongReal(0.005d0*ABS(FLOAT(toGo,LONGREAL)))));
              form.add("comments:");
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.text, 
                                             "comment"&rT));
              form.add("<br>\n");
              (****************************************)
            END(*WITH*)
              END(*FOR r*);
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                             name := "record_execution", 
                                             value := "RECORD TRADES"));

              p.page.addToBody(form)
            END;
            p.page.addToBody("<br><br>");
            WITH basketTab = Database.TExec(
  "select t.trading_id as basket_id, t.serial, t.type, t.ticker, t.state, t.count, t.target_price, flt8_mul_cash(t.bookcost,'$1') as bookcost, t.strategy from trading_details t where t.trading_id=" & Fmt.Int(basket_id) & " order by serial;"
              ) DO
              p.page.addToBody("<hr>\n");
              p.page.addToBody("<br><h3>BASKET TRADES:</h3><br>\n");
              p.page.addToBody(basketTab.allColsToHTML(format));
            END
             
            
          END
        END
      EXCEPT
        TextReader.NoMore =>
          HTML.Error("TextReader.NoMore exception in DoExecB while parsing \"" & exec & "\"")
      |
        Lex.Error, FloatMode.Trap => 
          HTML.Error("Trouble reading number in DoExecB while parsing \"" & exec & "\"")
      END
    ELSE
      HTML.Error("Got to DoExecB without an update??? Check your URL!", doQuit := TRUE)
    END;

  END DoExecB;

PROCEDURE ConfirmTrade(p : Page; request : Request.T) RAISES { DBerr.Error }=
  VAR
    rec, sharesT, priceT, commissionT, comment, ticker, trade_idT : TEXT;
    numTradesT : TEXT;
    ledger : TradingLedger.T := NIL;
    last_trade_id := LAST(CARDINAL);
  BEGIN
    p.page.setBgColor("pink");
    IF request.getPostVar("record_execution",rec) THEN
      TRY
        IF NOT request.getPostVar("num_trades",numTradesT) THEN
          HTML.Error("Incorrectly formatted trade request.")
        END;
        
        IF TE(rec, "CONFIRM TRADE") OR TE(rec, "CONFIRM TRADES") THEN
          ledger := NEW(TradingLedger.T).initFromDB(Database.Static()); 
          ledger.begin()
        END;

        FOR i := 0 TO Scan.Int(numTradesT)-1 DO
        IF NOT (request.getPostVar("shares"&Fmt.Int(i),sharesT) AND
                request.getPostVar("price"&Fmt.Int(i), priceT) AND
                request.getPostVar("commission"&Fmt.Int(i), commissionT) AND
                request.getPostVar("ticker"&Fmt.Int(i), ticker) AND
                request.getPostVar("comment"&Fmt.Int(i), comment) AND
                request.getPostVar("trade_id"&Fmt.Int(i), trade_idT)) THEN
          HTML.Error("Incorrectly formatted trade request.")
        END;
        
        WITH shares = Scan.Int(sharesT),
             price = Scan.LongReal(priceT),
             commissions = Scan.LongReal(commissionT),
             trade_id = Scan.Int(trade_idT) DO
          last_trade_id:=trade_id; (* dirty hack *)

          IF shares < 0 THEN
            p.page.addToBody("<strong>SLD</strong> ")
          ELSE
            p.page.addToBody("<strong>BOT</strong> ")
          END;
          p.page.addToBody(Fmt.Int(ABS(shares)) & " " & ticker & " <strong>@</strong> $" & 
            Fmt.LongReal(price, style := Fmt.Style.Fix, prec := 2) & " <strong>; commissions</strong> $" & Fmt.LongReal(commissions, style := Fmt.Style.Fix, prec := 2));
          p.page.addToBody("<br><br>\n");
          p.page.addToBody("<strong>Total change in cash:</strong> $" & Fmt.LongReal(-(FLOAT(shares,LONGREAL)*price+commissions), style := Fmt.Style.Fix, prec := 2));
          p.page.addToBody("<br><br>\n");
          p.page.addToBody("<strong>Comments:</strong><br>"&comment & "<br>\n");
          
          IF TE(rec, "RECORD TRADE")  THEN
            
            WITH form1 = NEW(HTMLForm.T).init(request.toPage, request),
                 form2 = NEW(HTMLForm.T).init("do_exec",request) DO
              
              request.addPostVarsAsHidden(form1);
              form1.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                              name := "record_execution",
                                              style := "color:blue",
                                              value := "CONFIRM TRADE"));
              
              WITH i1 = NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                              name := "exec",
                                              value := "trade " &
                                                           Fmt.Int(trade_id)),
                   i2 = NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                              name := "cancel",
                                              value := "Cancel") DO
                form2.add(i1); form2.add(i2)
              END;
              
              p.page.addToBody(HTMLTable.Horiz(form1,form2))
            END
          ELSIF TE(rec, "CONFIRM TRADE") OR TE(rec, "CONFIRM TRADES") THEN
            (* db data entry for execution... *)
              ledger.enterTrade(trade_id,ticker,shares,price,commissions,
                                comment,now);

              IF pw # NIL THEN
                TRY
                  WITH tradeRec = PriceWatch.Trade { dbName,
                                                     ticker,
                                                     trade_id } DO
                    pw.logExecution(tradeRec, price, shares)
                  END
                EXCEPT
                  PriceWatch.NoSuchTrade => 
                    Debug.Warning("No record in pricewatch database for trade \"" & dbName & "." & Fmt.Int(trade_id) & "\" of ticker \"" &
                      ticker & "\" (no entries recorded)")
                END
              END
          END
        END
        END(*FOR*);
        IF TE(rec, "CONFIRM TRADE") OR TE(rec, "CONFIRM TRADES") THEN
          ledger.commit();

              IF NOT TE(Database.TExec(
  "select tl.closing from trading_log tl, trading_details td"&
  "  where tl.serial=td.trading_id and td.serial="& Fmt.Int(last_trade_id) &";").getUniqueEntry("closing"),"") THEN

                WITH tickerTab = TradeBalanceTab(last_trade_id) DO

                  p.page.addToBody("<br><strong>REMAINING EXECUTIONS:");
                  p.page.addToBody("</strong><br>");
                  p.page.addToBody(tickerTab.allColsToHTML());
                  
                  
                  WITH stoxRem = BalanceTabBalance(tickerTab) DO
                    p.page.addToBody("<br>Out of balance by " & 
                      Fmt.Int(stoxRem) & " shares total.<br>");
                    
                    IF stoxRem = 0 THEN
                      p.page.addToBody("<br><strong>POSITION EXIT BALANCES "&
                        "ENTRY</strong><br>")
                    END
                  END
                END
              END(*IF*);

            p.page.addToBody(SiteNavBar(request));
    p.page.addToBody("<br><br>");
            p.page.addToBody("<h3>TRADE ENTRY CONFIRMED</h3>");

            WITH form = NEW(HTMLForm.T).init("redirect_holding_exec",request) DO
              form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit, 
                                               name := "no_op",
                                               value := "RETURN TO A&E PAGE"));
              p.page.addToBody(form)
            END
        END;

          IF TE(rec, "RECORD TRADES")  THEN
            
            WITH form1 = NEW(HTMLForm.T).init(request.toPage, request),
                 form2 = NEW(HTMLForm.T).init("do_exec",request) DO
              
              request.addPostVarsAsHidden(form1);
              form1.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                              name := "record_execution",
                                              style := "color:blue",
                                              value := "CONFIRM TRADES"));
              
              WITH i1 = NEW(HTMLInput.T).init(HTMLInput.Type.hidden,
                                              name := "exec",
                                              value := "trade_b"),
                   i2 = NEW(HTMLInput.T).init(HTMLInput.Type.submit,
                                              name := "cancel",
                                              value := "Cancel") DO
                form2.add(i1); form2.add(i2)
              END;
              
              p.page.addToBody(HTMLTable.Horiz(form1,form2))
            END
          END
      EXCEPT
        FloatMode.Trap, Lex.Error => 
          HTML.Error("Trouble parsing numerical value in trade data.  Re-check your data entry.")
      END
    END;
    p.page.addToBody("\n<br><br><br>\n");
  END ConfirmTrade;

TYPE
  MyFormat = HTMLFormatting.Dynamic OBJECT
    request : Request.T;
  OVERRIDES
    format := MFFormat;
  END;

  MyFormat2 = HTMLFormatting.DynamicTD OBJECT
    request : Request.T;
    printed := ARRAY Button OF IntSet.T { NIL, .. };
    targetPage : TEXT;
  OVERRIDES
    format := ActionFormat;
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

TYPE 
  Button = { Accept, Reject, Finalize, Reverse, AckCancel, ForceExit, AlreadyExiting, ExitRejected, MoveHistorical, TradeBasket };
  B = Button;
  Buttons = SET OF B;
CONST
  BNames = ARRAY B OF TEXT { "ACCEPT", "REJECT", "FINALIZE", "REVERSE", "ACKNOWLEDGE_CANCEL", "FORCE EXIT", "EXIT IN PROGRESS", "EXIT REJECTED", "SET HISTORICAL", "TRADE_B" };
  
PROCEDURE ActionFormat(m : MyFormat2; in : HTML.Stuff) : HTML.Stuff 
  RAISES { DBerr.Error } =
  <*FATAL FloatMode.Trap, Lex.Error *>

  PROCEDURE White(s : HTML.Stuff) : HTML.Stuff =
    BEGIN
      WITH list = NEW(HTMLList.T) DO
        list.add("<td bgcolor=\"#ffffff\">");
        list.add(s);
        list.add("</td>");
        RETURN list
      END
    END White;

  CONST
    BStyles = ARRAY B OF TEXT { "color:green", "color:red", NIL, "color:red", "color:red", "color:red", NIL, NIL, "color:green", "color:blue" };
    NonButtons = SET OF B { B.AlreadyExiting, B.ExitRejected };
  VAR
    buttons : Buttons;
  BEGIN
    IF m.printed[FIRST(B)] = NIL THEN
      FOR b := FIRST(B) TO LAST(B) DO
        m.printed[b] := NEW(IntSetDef.T).init()
      END
    END;

    TYPECASE in OF 
      TEXT(txt) =>
      IF IsChars(txt, SET OF CHAR { '0'..'9' } ) THEN
        (* look up state and basket_id of trade *)
        WITH tab = Database.TExec(
  "select state,trading_id,closing from trading_details td, trading_log tl"&
  "  where td.trading_id=tl.serial and td.serial=" & txt & ";"
          ),
           basket_id = Scan.Int(tab.getUniqueEntry("trading_id")),
           state = tab.getUniqueEntry("state"),
           isClosing = NOT TE(tab.getUniqueEntry("closing"),""),
           pendOK = isClosing AND TE(Database.TExec(
  "select distinct td.state='executed' from trading_details td, trading_log tl where td.trading_id=tl.serial and tl.serial="&tab.get("closing")&";").getI(0),"t")
         DO
          IF TE(state, "pending") THEN
            buttons := Buttons { B.Reject, B.TradeBasket }
          ELSIF TE(state, "holding") AND (isClosing AND NOT pendOK)  THEN
            buttons := Buttons { B.Reject }
          ELSIF TE(state, "holding") THEN
            buttons := Buttons { B.Accept, B.Reject }
          ELSIF TE(state, "partexec") OR TE(state, "urgent") THEN
            (* we may finalize a position at any time if it is an opening
               position; we may only finalize it if it is balanced against
               its corresponding opening position if it is a closing position*)
            IF isClosing AND 
               BalanceTabBalance(TradeBalanceTab(Scan.Int(txt))) #0 THEN
              buttons := Buttons { B.Reverse }
            ELSE
              buttons := Buttons { B.Finalize, B.Reverse }
            END
          ELSIF TE(state, "pending") THEN
            buttons := Buttons { B.Reject }
          ELSIF TE(state, "canceling") THEN
            buttons := Buttons { B.AckCancel }
          ELSIF TE(state, "rejected") OR TE(state, "canceled") OR TE(state, "reversed") THEN
            buttons := Buttons { B.MoveHistorical }
          ELSIF TE(state, "executed") AND NOT isClosing THEN
            WITH numExits = Scan.Int(Database.TExec(
  "select count(*) from trading_log where active and closing=" & Fmt.Int(basket_id) & ";"
              ).getUniqueEntry("count")) DO
              IF numExits = 0 THEN
                buttons := Buttons { B.ForceExit }
              ELSE
                WITH rejected = Scan.Int(Database.TExec(
  "select count(*) from trading_details, trading_log tl where state='rejected' and tl.closing="&Fmt.Int(basket_id)&" and tl.active and tl.serial=trading_details.trading_id;"
                  ).getUniqueEntry("count")) DO
                  IF rejected > 0 THEN
                    buttons := Buttons { B.ExitRejected }
                  ELSE
                    buttons := Buttons { B.AlreadyExiting }
                  END
                END
              END
            END
          ELSIF TE(state, "executed") AND isClosing THEN
            buttons := Buttons { B.MoveHistorical }
          ELSIF TE(state, "historical") OR TE(state,"canceled") THEN
            (* only way we can get here is if the state of the trade changed between
               the time we picked it and now *)
            buttons := Buttons { }
          ELSE
            HTML.Error("ActionFormat: unknown state \"" & state & "\" for serial "&txt)
          END;

          FOR b := FIRST(B) TO LAST(B) DO
            IF b IN buttons AND NOT m.printed[b].member(basket_id) THEN
              EVAL m.printed[b].insert(basket_id);
              IF b IN NonButtons THEN
                RETURN White(BNames[b])
              ELSE
                WITH form = NEW(HTMLForm.T).init(m.targetPage,m.request) DO
                  form.add(NEW(HTMLInput.T).init(HTMLInput.Type.submit, 
                                                 name := "update",
                                                 value := BNames[b] & " " & Fmt.Int(basket_id) & "",
                                                 style := BStyles[b]));
                  RETURN White(form)
                END
              END
            END
          END; (* FOR b := ... *)
          RETURN White("")
        END;
      ELSE
        RETURN White(txt)
      END
    ELSE
      Process.Crash("Calling ActionFormat on non-TEXT!");
      <* ASSERT FALSE *>
    END
  END ActionFormat;

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

PROCEDURE TradesFormatting(request : Request.T;
                           targetPage := "holding_exec") : HTMLFormatting.T =
  BEGIN

      RETURN NEW(MyFormat, 
                 request := request, 
                      column := "trade",
            next := NEW(MyFormat2, 
                        request := request,
                        column := "basket_action",
                        targetPage := targetPage,
              next := NEW(MyFormat3,
                          request := request,
                          column := "basket_id",
                next := NEW(FixNumFmt, 
                            column := "bookcost",
                next := NEW(FixNumFmt, 
                            column := "ave_price",
                next := NEW(FixNumFmt, 
                            column := "accumulated_pl", prec := 2,
                next := NEW(FixNumFmt, 
                            column := "closing_target",
                  next := NEW(FixNumFmt,
                              column := "target_price",
                              next := MakeAlignments(ARRAY OF TEXT {
      "bookcost", "accumulated_pl", "serial", "count", "target_price", 
      "executed_count", "so_far", "ave_price", "closing_trade", "closing_target"
      } )))))))))
      
  END TradesFormatting;

PROCEDURE FillInExecPage(list : HTMLList.T; 
                         request : Request.T)  RAISES { DBerr.Error } =



  CONST
    hlSize = "H3";
  VAR
    q := NEW(REF TEXT);
  BEGIN
      list.add("<"&hlSize&">URGENT TRADES---</"&hlSize&">\n");
      WITH tab = GetTrades(Restriction.UrgentOrPartExec,
                           calcSoFar := TRUE,
                           debugQuery := q).allColsToHTML(TradesFormatting(request), RowFormatting()) DO
        list.add(tab);
        list.add("<br>")
      END;
      list.add("\n<hr>\n");
      list.add("<"&hlSize&">PENDING TRADES---</"&hlSize&">\n");

      WITH tab = GetTrades(Restriction.Pending).allColsToHTML(TradesFormatting(request),RowFormatting()) DO
        list.add(tab);
        list.add("<br>")
      END;
      WITH tab = GetTrades(Restriction.Canceling).allColsToHTML(TradesFormatting(request),RowFormatting()) DO
        list.add(tab);
      END;
    list.add("<"&hlSize&">---END</"&hlSize&">\n");
  END FillInExecPage;

PROCEDURE CheckSignin(p : Page; request : Request.T)  RAISES { DBerr.Error }  = <* FATAL FloatMode.Trap, Lex.Error *>
  VAR
    login, password : TEXT;
    result : Database.Table;
  BEGIN
    IF NOT request.getPostVar("login", login) OR
       NOT request.getPostVar("password", password) THEN
      HTML.Error("unknown error", doQuit := TRUE );
    END;
    
    result := Database.TExec(
                  "SELECT pass, name, status, uid FROM userinfo WHERE email='" &
                  DatabaseUtils.Sanitize(login) & "'");

    IF result.getNumRows() = 0 THEN
      HTML.Error("unknown login id " & login) 
    ELSIF result.getNumRows() > 1 THEN
      HTML.Error("Database integrity problem: multiple rows for login id" &
        login &", please contact database administrator.")
    ELSIF NOT TE(password, result.getUniqueEntry("pass")) THEN
      HTML.Error("Password mismatch, please try again.")
    ELSE

      (* at this point, we need to create a session for the user *)
      VAR
        uid := Scan.Int(result.getUniqueEntry("uid"));
        remoteAddr : TEXT;
      BEGIN
        IF NOT request.getEnvVar("REMOTE_ADDR", remoteAddr) THEN
          HTML.Error("REMOTE_ADDR not defined in environment!");
        END;
        request.session := NEW(Session.T).init(uid, remoteAddr);
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
      END
    END;
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

PROCEDURE GCDisplay(p : Page; req : Request.T) : HTMLPage.T RAISES { DBerr.Error } =
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
  BEGIN now := FindFakeTime() END InitDB;

PROCEDURE InitPW(recordHostName, tDb : TEXT) RAISES { DBerr.Error } =
  BEGIN pw := NEW(PriceWatch.T).init(recordHostName); dbName:=tDb  END InitPW;

VAR
  dbName : TEXT;
  now := Ledger.NowIsNow;
  CGIname := Pathname.Last(Params.Get(0));
  pw : PriceWatch.T := NIL;
BEGIN 
  Pages.AddDispatch ( "main", 
                      NEW(Page, body := Main, title := "Navigation Page") );

  Pages.AddDispatch ( "signin", NEW(Page, body := Signin ),
                      signinNeeded := FALSE);
  Pages.AddDispatch ( "check_signin", NEW(Page, body := CheckSignin ),
                      signinNeeded := FALSE );

  Pages.AddDispatch( "balance_sheet", NEW(Page, body := BalanceSheet, title := "Balance Sheet" ) );
  Pages.AddDispatch( "holdings", NEW(Page, body := Holdings, title := "Journal" ) );
  Pages.AddDispatch( "redirect_holdings", NEW(RedirPage, 
                                              target:= "holdings"));

  (* comb of state_holding & exec_page : *)
  Pages.AddDispatch( "holding_exec", NEW(Page, body := HoldingExec, 
                                         headerStuff := TinyTextHeader,
                                         title := "Approval & Execution" ));
  Pages.AddDispatch( "redirect_holding_exec", NEW(RedirPage, 
                                                  target := "holding_exec"));

  Pages.AddDispatch( "redirect_state_executed", NEW(RedirPage, 
                                                  target := "state_executed"));

  Pages.AddDispatch( "do_exec", NEW(Page, body := DoExec, title := "Trade Entry Page" ) );
  Pages.AddDispatch( "redirect_do_clear", NEW(RedirPage, 
                                                  target := "do_clear"));


  Pages.AddDispatch( "confirm_trade", NEW(Page, body := ConfirmTrade, title := "CONFIRM TRADE" ) );
  Pages.AddDispatch( "state_executed", NEW(Page, body := StateExecuted, title := "Current Positions" ) );
  Pages.AddDispatch( "pl_log", NEW(Page, body := PLLog, title := "Profit and Loss Log" ) );
  Pages.AddDispatch( "state_rejected", NEW(Page, body := StateRejected, title := "Rejected Orders" ) );
  Pages.AddDispatch( "do_clear", NEW(Page, body := DoClear, title := "Orders to Clear" ) );

  Pages.AddDispatch( "perform_action", NEW(Page, body := PerformAction));

  (* admin pages *)
  Pages.AddDispatch ( "admin_main", NEW(Page, body := AdminMain, privLevel := Pages.Priv.Admin, title := "Administrator Main" ) );
  Pages.AddDispatch ( "list_users", NEW(Page, body := ListUsers, privLevel := Pages.Priv.Admin, title := "User List" ) );
  Pages.AddDispatch ( "run_sql", NEW(Page, body := RunSQL, privLevel := Pages.Priv.Admin, title := "Run SQL Query" ) );
  
END GCSite.
