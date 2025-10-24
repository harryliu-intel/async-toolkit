MODULE DsimRuleFormat EXPORTS Dsim;
IMPORT Wx, Name;
IMPORT NameSeq;
FROM Fmt IMPORT F;

PROCEDURE FormatRule(rule : Rule) : TEXT =

  PROCEDURE FormatConjunct(c : Conjunct) =
    BEGIN
      IF c.sense = Sense.Down THEN Wx.PutChar(wx, '~') END;
      Wx.PutText(wx, Name.Format(c.input))
    END FormatConjunct;

  VAR
    wx := Wx.New();
    p := rule.conjuncts;
  BEGIN
    FOR i := FIRST(Keyword) TO LAST(Keyword) DO
      IF i IN rule.attrs AND i # Keyword.After THEN
        Wx.PutText(wx, KeywordTexts[i]);
        Wx.PutChar(wx, ' ')
      END
    END;

    IF rule.delay # LAST(CARDINAL) THEN
      Wx.PutText(wx, "after ");
      Wx.PutInt(wx, rule.delay);
      Wx.PutChar(wx, ' ')
    END;

    WHILE p # NIL DO
      FormatConjunct(p.head);

      IF p.tail # NIL THEN Wx.PutText(wx, " & ") END;

      p := p.tail
    END;

    Wx.PutText(wx, " -> ");
    Wx.PutText(wx, Name.Format(rule.target));
    CASE rule.sense OF
      Sense.Up   => Wx.PutChar(wx, '+')
    |
      Sense.Down => Wx.PutChar(wx, '-')
    END;
    RETURN Wx.ToText(wx)
  END FormatRule;

PROCEDURE FormatDefine(d : Define) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, F("typeName %s\n", Name.Format(d.typeName)));
    WxNameSeq(wx, d.args);
    Wx.PutText(wx, "\n...\n");
    RETURN Wx.ToText(wx)
  END FormatDefine;

PROCEDURE WxNameSeq(wx : Wx.T; seq : NameSeq.T) =
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH nm = seq.get(i) DO
        Wx.PutText(wx, F("%s ", Name.Format(nm)))
      END
    END
  END WxNameSeq;
BEGIN END DsimRuleFormat.
