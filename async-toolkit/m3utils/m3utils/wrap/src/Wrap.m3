(* $Id$ *)

MODULE Wrap;
IMPORT Wx;
IMPORT Rd;
IMPORT Text, TextUtils;
IMPORT TextSet, TextSetDef;
IMPORT Thread;
IMPORT Debug;
IMPORT Fmt;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;

PROCEDURE ReadTillEnd(rd : Rd.T) : TEXT RAISES { Rd.Failure } =
  CONST 
    BufSize = 8*1024;

  VAR
    wx := Wx.New();
    buff : ARRAY [0..BufSize-1] OF CHAR;
    n : CARDINAL;
  BEGIN
    REPEAT 
      n := Rd.GetSub(rd, buff);
      Wx.PutStr(wx, SUBARRAY(buff, 0, n))
    UNTIL n < BufSize;

    RETURN Wx.ToText(wx)
  END ReadTillEnd;

PROCEDURE FilterBetweenAts(txt    : TEXT; 
                           symbol : TEXT; 
                           keep   : BOOLEAN) : TEXT =
  VAR
    sTarget := "@"  & symbol & ">@";
    eTarget := "@<" & symbol & "@";
    nsTarget := "@!"  & symbol & ">@";
    neTarget := "@<!"  & symbol & "@";
  BEGIN
    RETURN FilterBetween(FilterBetween(txt, sTarget, eTarget, keep),
                         nsTarget, neTarget, NOT keep)
  END FilterBetweenAts;

PROCEDURE FilterBetween(txt              : TEXT; 
                        sTarget, eTarget : TEXT; 
                        keep             : BOOLEAN) : TEXT =
  VAR
    stLen := Text.Length(sTarget);
    etLen := Text.Length(eTarget);
    o : CARDINAL := 0;
    p : CARDINAL;
    wx := Wx.New();
    len := Text.Length(txt);
  BEGIN
    REPEAT

      (* invariant: we have processed all of string before character #o *)
         
      IF TextUtils.FindSub(txt, sTarget, p, o) THEN
        (* copy prefix of string (prior to sTarget) *)
        Wx.PutText(wx, Text.Sub(txt, o, p-o));

        (* skip target *)
        o := p + stLen;

        (* search for eTarget *)
        IF NOT TextUtils.FindSub(txt, eTarget, p, o) THEN
          (* not found, we use the rest of the string *)
          p := len
        END;

        (* conditionally copy selected string *)
        IF keep THEN Wx.PutText(wx, Text.Sub(txt, o, p-o)) END;

        (* o becomes the first character after eTarget *)
        o := MIN(len, p + etLen)
      ELSE
        (* no sTarget, pass the rest thru *)
        Wx.PutText(wx, Text.Sub(txt, o)); 
        o := len
      END
    UNTIL o = len;
    RETURN Wx.ToText(wx)
  END FilterBetween;

PROCEDURE FindTargets(txt, symbol, pfx, sfx : TEXT) : TextSet.T =
  VAR
    len := Text.Length(txt);
    targ := pfx & symbol & "=";
    tLen := Text.Length(targ);
    s, p : CARDINAL := 0;
    set := NEW(TextSetDef.T).init();

    pLen := Text.Length(pfx);
    sLen := Text.Length(sfx);
    
  BEGIN
    LOOP
      IF TextUtils.FindSub(txt, targ, p, p) THEN
        INC(p, tLen);
        s := p;
        WHILE p < len AND NOT TE(Text.Sub(txt, p, sLen), sfx) DO 
          INC(p)
        END;
        (* txt[p] = sfx OR p = EOT *)
        IF p # len THEN INC(p,sLen) END;
        (* txt[p] = past string of interest *)
        WITH val = targ & Text.Sub(txt, s, p-s),
             vLen = Text.Length(val),
             chopped = Text.Sub(val, pLen, vLen - pLen - sLen) DO
          Debug.Out("Found target \"" & val & "\" -> \"" & chopped & "\"");
          EVAL set.insert(chopped)
        END
      ELSE
        EXIT
      END
    END;
    RETURN set
  END FindTargets;

PROCEDURE SelectBetweenAts(txt    : TEXT; 
                           symbol : TEXT; 
                           val    : TEXT) : TEXT =
  VAR
    targets := FindTargets(txt, symbol, "@", ">@").union(
               FindTargets(txt, symbol, "@!", ">@"));
    myTarg  := symbol & "=" & val;
    iter := targets.iterate();
    targ : TEXT;
  BEGIN
    WHILE iter.next(targ) DO
      WITH keep = TE(targ, myTarg) DO
        Debug.Out("Filtering \"" & targ & "\" keep=" & Fmt.Bool(keep));
        txt := FilterBetweenAts(txt, targ, keep)
      END
    END;
    RETURN txt
  END SelectBetweenAts;

BEGIN
END Wrap.
