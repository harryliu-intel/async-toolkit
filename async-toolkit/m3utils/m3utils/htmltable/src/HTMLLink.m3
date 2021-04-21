MODULE HTMLLink;
IMPORT HTML;
IMPORT TextUtils AS Utils;
IMPORT Request;
IMPORT Pathname, Params;
IMPORT Debug;
FROM Fmt IMPORT F, Bool;
IMPORT TextTextTbl;

REVEAL
  T = Public BRANDED "HTML Link" OBJECT
    myURL : TEXT;
    text : TEXT;
    encloses : HTML.T;
  OVERRIDES
    init := Init;
    format := Format;
    URL := GetURL;
  END;

PROCEDURE MakeURL(to      : TEXT;
                  from    : Request.T ;
                  local   : BOOLEAN;
                  getVars : TextTextTbl.T) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    Debug.Out(F("HTMLLink.MakeURL: local=%s (getVars=NIL)=%s",
                Bool(local), Bool(getVars=NIL)), 0);
    IF local THEN
      res := CGIname & "?" & to ;
      IF from # NIL AND from.toPage # NIL THEN
        res := res & "?" & from.toPage;
        IF from.session # NIL THEN
          res := res & "?" & from.session.getId() 
        END
      END;
      IF getVars # NIL THEN
        VAR
          iter := getVars.iterate();
          k, v : TEXT;
          first := TRUE;
        BEGIN
          WHILE iter.next(k, v) DO
            Debug.Out(F("HTMLLink.MakeURL : var %s <- %s", k, v), 0);
            IF first THEN
              res := res & "?";
              first := FALSE
            ELSE
              res := res & "&"
            END;
            res := res & k & "=" & v
          END
        END
      END
    ELSE
      res := to
    END;
    RETURN res;
  END MakeURL;

PROCEDURE Init(self     : T; 
               encloses : HTML.Stuff;
               to       : TEXT;
               from     : Request.T;
               local    : BOOLEAN;
               getVars  : TextTextTbl.T) : T =
  BEGIN
    self.encloses := HTML.Wrap(encloses);
    self.myURL := MakeURL(to, from, local, getVars);
    self.text := "<a href=\"" & self.myURL & "\">\n";    
    RETURN self
  END Init;

PROCEDURE Format(self : T) : TEXT =
  VAR
    res : TEXT;
  BEGIN
    res := self.text & self.encloses.format() & "\n</a>";
    RETURN res
  END Format;

PROCEDURE GetURL(t : T) : TEXT = BEGIN RETURN t.myURL END GetURL;

(* helpful stuff for building SQL queries *)

PROCEDURE QueryField(request : Request.T;   (* current request *)
                     to : TEXT;                (* link to *)
                     encloses : TEXT;          (* text linked from *)
                     SQLfield : TEXT;          (* name of SQL field *)
                     local : BOOLEAN
                     ) : TEXT = 
  VAR
    res := "";
  BEGIN
    res := res & "'<a href=\"";
    IF local THEN 
      res := res & CGIname & "?" & to & "?" & request.toPage &
                       "?" & request.session.getId() & "?input="
    ELSE
      res := res & to 
    END;
    res := res & "'||" & SQLfield & "||'\">";
    
    (* here we should look for the escape character.*) 
    (* If present, replace with an SQL field as before *)
    
    IF Utils.CountChars(encloses, '%') > 0 THEN
      VAR 
        beginning, sqlId, end : TEXT;
      BEGIN
        Utils.SplitText(encloses,'%', beginning, end);
        Utils.SplitText(end, '%', sqlId, end);
        res := res & beginning & "'||" & sqlId & "||'</a>'";
        IF end # NIL THEN 
          res := res & "||'" & end & "</a>'"
        END;
      END
    ELSE
      res := res & encloses & "</a>'"
    END;
    RETURN res
  END QueryField;

VAR
  CGIname := Pathname.Last(Params.Get(0));
BEGIN END HTMLLink.


