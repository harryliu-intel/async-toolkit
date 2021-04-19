(* $Id$ *)

MODULE HTMLPage;
IMPORT HTML;
IMPORT Debug;
IMPORT HTMLList;
IMPORT Fmt;

REVEAL
  T = Public BRANDED Brand OBJECT
    header : TEXT := "";
    body : HTML.T := NIL;
    footer : HTML.T := NIL;
    scripts := "";
    extraBodyTags := "";
    bgColor := "#ffffff";
  OVERRIDES
    setFooter := SetFooter;
    setHead := SetHead;
    setBody := SetBody;
    addToBody := AddToBody;
    format := Format;

    setBgColor := SetBgColor;
    addScript := AddScript;
    addBodyTag := AddBodyTag;

    addJavaScriptRefresh := AddJavaScriptRefresh;
    renderHeader := RenderHeader;
  END;

PROCEDURE AddJavaScriptRefresh(t : T; ms : CARDINAL ) = 
  BEGIN
    t.addScript(


"<script language=\"JavaScript\">\n" &
"<!--\n" &
"\n" &
"var sURL = unescape(window.location.pathname);\n" &
"\n" &
"function doLoad()\n" &
"{\n" &
"    // the timeout value should be the same as in the \"refresh\" meta-tag\n" &
"    setTimeout( \"refresh()\", "&Fmt.Int(ms)&" );\n" &
"}\n" &
"\n" &
"function refresh()\n" &
"{\n" &
"    //  This version of the refresh function will cause a new\n" &
"    //  entry in the visitor's history.  It is provided for\n" &
"    //  those browsers that only support JavaScript 1.0.\n" &
"    //\n" &
"    window.location.href = sURL;\n" &
"}\n" &
"//-->\n" &
"</script>\n" &
"\n" &
"<script language=\"JavaScript1.1\">\n" &
"<!--\n" &
"function refresh()\n" &
"{\n" &
"    //  This version does NOT cause an entry in the browser's\n" &
"    //  page view history.  Most browsers will always retrieve\n" &
"    //  the document from the web-server whether it is already\n" &
"    //  in the browsers page-cache or not.\n" &
"    //  \n" &
"    window.location.replace( sURL );\n" &
"}\n" &
"//-->\n" &
"</script>\n" &
"\n" &
"<script language=\"JavaScript1.2\">\n" &
"<!--\n" &
"function refresh()\n" &
"{\n" &
"    //  This version of the refresh function will be invoked\n" &
"    //  for browsers that support JavaScript version 1.2\n" &
"    //\n" &
"    \n" &
"    //  The argument to the location.reload function determines\n" &
"    //  if the browser should retrieve the document from the\n" &
"    //  web-server.  In our example all we need to do is cause\n" &
"    //  the JavaScript block in the document body to be\n" &
"    //  re-evaluated.  If we needed to pull the document from\n" &
"    //  the web-server again (such as where the document contents\n" &
"    //  change dynamically) we would pass the argument as 'true'.\n" &
"    //  \n" &
"    window.location.reload( false );\n" &
"}\n" &
"//-->\n" &
"</script>\n" &
"");
    t.addBodyTag("onload=\"doLoad()\"") 
  END AddJavaScriptRefresh;

PROCEDURE SetBgColor(t : T; c : TEXT) = BEGIN t.bgColor := c END SetBgColor;

PROCEDURE AddScript(t : T; sc : TEXT) = 
  BEGIN t.scripts := t.scripts & "\n" & sc END AddScript;

PROCEDURE AddBodyTag(t : T; td : TEXT) =
  BEGIN t.extraBodyTags := t.extraBodyTags & td END AddBodyTag;

PROCEDURE SetFooter(self : T; footer : HTML.Stuff) =
  BEGIN self.footer := HTML.Wrap(footer) END SetFooter;

PROCEDURE SetHead(self : T; headerText : TEXT) : T =
  BEGIN self.header := headerText; RETURN self END SetHead;

PROCEDURE SetBody(self : T; body : HTML.Stuff) : T =
  BEGIN self.body := HTML.Wrap(body); RETURN self END SetBody;

PROCEDURE AddToBody(self: T; stuff : HTML.Stuff) =
  BEGIN
    IF self.body = NIL THEN EVAL SetBody(self,stuff) 
    ELSE
      VAR
        newbody := NEW(HTMLList.T);
      BEGIN
        newbody.add(self.body);
        newbody.add(stuff);
        self.body := newbody
      END
    END
  END AddToBody;

PROCEDURE RenderHeader(self : T) : TEXT =
  BEGIN RETURN self.header END RenderHeader;

PROCEDURE Format(self : T) : TEXT =
  VAR
    res := "";
  BEGIN

    res := "<head>" & self.renderHeader() & "</head>\n" & 
               self.scripts &
               "<body bgcolor=" & self.bgColor & " " & self.extraBodyTags & ">" ;

    Debug.Out("HTMLPage.Format formatting body.");
    IF self.body # NIL THEN res := res & self.body.format() END;

    Debug.Out("HTMLPage.Format formatting footer.");
    IF self.footer # NIL THEN res := res & self.footer.format() END;
    res := res & "</body>\n";
    
    RETURN res
  END Format;

REVEAL
  FrameSet = PubFrameSet BRANDED Brand & " FrameSet" OBJECT
    direction : Direction;
    widths : REF ARRAY OF LONGREAL := NIL;
    data : REF ARRAY OF Frame;
  METHODS
    renderFrameSet() : TEXT := FSRenderFrameSet;
  OVERRIDES
    setFooter := AbortSetFooter;
    setBody := AbortSetBody;
    addToBody := AbortAddToBody;
    setBgColor := AbortSetBgColor;
    addBodyTag := AbortAddBodyTag;
    addScript := AbortAddScript;
    
    init := FSInit;
    addElement := FSAddElement;
    format := FSFormat;
  END;

PROCEDURE AbortSetFooter(<*UNUSED*>fs : FrameSet;
                         <*UNUSED*>st : HTML.Stuff) = 
  BEGIN <* ASSERT FALSE *> END AbortSetFooter;

PROCEDURE AbortSetBody(<*UNUSED*>fs : FrameSet;
                         <*UNUSED*>st : HTML.Stuff) : T = 
  BEGIN <* ASSERT FALSE *> END AbortSetBody;

PROCEDURE AbortAddToBody(<*UNUSED*>fs : FrameSet;
                         <*UNUSED*>st : HTML.Stuff) = 
  BEGIN <* ASSERT FALSE *> END AbortAddToBody;

PROCEDURE AbortAddBodyTag(<*UNUSED*>fs : FrameSet;
                         <*UNUSED*>tag : TEXT) =
  BEGIN <* ASSERT FALSE *> END AbortAddBodyTag;

PROCEDURE AbortSetBgColor(<*UNUSED*>fs : FrameSet;
                         <*UNUSED*>tag : TEXT) =
  BEGIN <* ASSERT FALSE *> END AbortSetBgColor;

PROCEDURE AbortAddScript(<*UNUSED*>fs : FrameSet;
                         <*UNUSED*>tag : TEXT) =
  BEGIN <* ASSERT FALSE *> END AbortAddScript;

PROCEDURE FSInit(fs : FrameSet; 
                 direction : Direction;
                 n : CARDINAL;
                 widths : REF ARRAY OF LONGREAL) : FrameSet =
  BEGIN
    fs.direction := direction;
    fs.data := NEW(REF ARRAY OF Frame, n);
    IF widths # NIL THEN
      <* ASSERT NUMBER(widths^) = n *>
      fs.widths := NEW(REF ARRAY OF LONGREAL,
                       n);

      VAR
        sum := 0.0d0;
      BEGIN
        FOR i := FIRST(widths^) TO LAST(widths^) DO
          sum := sum + widths[i]
        END;

        FOR i := FIRST(widths^) TO LAST(widths^) DO
          fs.widths[i] := widths[i]/sum
        END
      END
    END;
    RETURN fs
  END FSInit;

PROCEDURE FSAddElement(fs : FrameSet; i : CARDINAL; elem : Frame) =
  BEGIN fs.data[i] := elem END FSAddElement;

PROCEDURE FSRenderFrameSet(fs : FrameSet) : TEXT =
  VAR
    res := "<frameset ";
  BEGIN

    IF fs.direction = Direction.Rows THEN
      res := res & "rows"
    ELSE
      res := res & "cols"
    END;

    IF fs.widths # NIL THEN
      res := res & "=\"";
      FOR i := FIRST(fs.widths^) TO LAST(fs.widths^) DO
        res := res & Fmt.Int(ROUND(fs.widths[i]*100.0d0)) & "%";
        IF i # LAST(fs.widths^) THEN
          res := res & ", "
        END
      END;
      res := res & "\""
    END;

    res := res & ">\n";

    (* print the actual frames *)

    FOR i := FIRST(fs.data^) TO LAST(fs.data^) DO
      WITH f = fs.data[i] DO
        TYPECASE f OF 
          Nested(n) => res := res & n.set.renderFrameSet()
        |
          URL(u) => res := res & "<frame src=\"" & u.url & "\">\n"
        ELSE
          <* ASSERT FALSE *>
        END
      END
    END;
    
    res := res & "</frameset>\n";
    RETURN res
  END FSRenderFrameSet;

PROCEDURE FSFormat(fs : FrameSet) : TEXT =
  VAR
    res := "<head>" & fs.header & "</head>\n";
  BEGIN
    res := res & fs.renderFrameSet(); 
    RETURN res
  END FSFormat;

BEGIN END HTMLPage.





