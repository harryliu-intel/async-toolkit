MODULE Fileviewer;
IMPORT DBerr;
IMPORT HTMLPage, HTMLLink, HTMLList;
IMPORT Pages, Debug, Text, Session, Scan;
IMPORT Fmt; FROM Fmt IMPORT Bool, F, Int;
IMPORT Request;
IMPORT PageDispatch;
IMPORT HTMLFormatting; 
IMPORT Process;
IMPORT HTMLTable;
IMPORT Thread;
IMPORT XTime AS Time;
IMPORT Pathname, Params;
IMPORT TextTable;
IMPORT DatabaseClass; (* to get directly at DatabaseTable.T.data *)
IMPORT HTMLSeq;
IMPORT OSError, Rd, FileRd;
IMPORT Wx;
IMPORT AL;
IMPORT FS;

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

PROCEDURE Make1Var(k, v : TEXT) : TextTable.T =
  VAR
    res := NEW(TextTable.T).init();
  BEGIN
    EVAL res.put(k, v);
    RETURN res
  END Make1Var;

PROCEDURE CopyVars(tbl : TextTable.T) : TextTable.T =
  VAR
    res := NEW(TextTable.T).init();
    k, v : TEXT;
    iter := tbl.iterate();
  BEGIN
    WHILE iter.next(k, v) DO
      EVAL res.put(k, v)
    END;
    RETURN res
  END CopyVars;
  
  (**********************************************************************)
  
PROCEDURE HTMLize(txt : TEXT) : TEXT =
  CONST
    HtmlChars = SET OF CHAR { '0' .. '9',
                              ' ',
                              'a' .. 'z',
                              'A' .. 'Z' };
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := 0 TO Text.Length(txt) - 1 DO
      WITH c = Text.GetChar(txt, i) DO
        IF c IN HtmlChars THEN
          Wx.PutChar(wx, c)
        ELSE
          Wx.PutText(wx, charTab[c])
        END
      END
    END;
    RETURN Wx.ToText(wx)
  END HTMLize;
  
PROCEDURE ViewFile(p : Page; request : Request.T) =
  VAR
    path : Pathname.T;
    v : TEXT;
  BEGIN
    Debug.Out("ViewFile!");
    
    IF request.getGetVars().get("path", v) THEN
      path := v;

      Debug.Out("ViewFile path : " & path);

      TRY
        WITH rd = FileRd.Open(path) DO
          LOOP
            WITH line = Rd.GetLine(rd) DO
              p.page.addToBody(Typewriter(HTMLize(line)));
              p.page.addToBody("<br>\n")
            END
          END
        END
      EXCEPT
        Rd.EndOfFile =>
      |
        OSError.E(x) =>
        Debug.Out(F("File not found: %s", path));
        p.page.addToBody(Bold(HTMLize(F("File \"%s\" not found : %s", path, AL.Format(x))) & "<br><br>\n"));

        DebugDumpDir(p, "/");
        DebugDumpDir(p, "/tmp");
        DebugDumpDir(p, ".");

      |
        Rd.Failure(x) =>
        p.page.addToBody(Bold(HTMLize(F("I/O error reading \"%s\" : %s", path, AL.Format(x))) & "<br><br>\n"))
      END
      
    END

  END ViewFile;

PROCEDURE DebugDumpDir(p : Page; path : Pathname.T) =
  VAR
    iter : FS.Iterator;
    name : TEXT;
  BEGIN
    p.page.addToBody("<br><br>Directory " & HTMLize(path) & "<br> <br>\n");

    TRY
      iter := FS.Iterate(path);
      WHILE iter.next(name) DO
        p.page.addToBody(HTMLize(name) & "<br>" & "\n")
      END
    EXCEPT
      OSError.E(x) =>  p.page.addToBody(Bold(HTMLize(F("Dir \"%s\" not found : %s", path, AL.Format(x))) & "<br><br>\n"))
    END
  END DebugDumpDir;

PROCEDURE Bold(t : TEXT) : TEXT =
  BEGIN RETURN F("<b>%s</b>", t) END Bold;
  
PROCEDURE Typewriter(t : TEXT) : TEXT =
  BEGIN RETURN F("<tt>%s</tt>", t) END Typewriter;
  

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
  charTab : ARRAY CHAR OF TEXT;
BEGIN

  Pages.AddDispatch ( "viewfile",
                      NEW(Page, body := ViewFile ),
                      signinNeeded := FALSE);

  FOR c := FIRST(CHAR) TO LAST(CHAR) DO
    charTab[c] := F("&#x%s;", Int(ORD(c),base := 16))
  END
END Fileviewer.
