MODULE TextSubs;
IMPORT Text;
IMPORT TextSubsClause;
IMPORT TextSubsClauseList;
IMPORT TextSubsClauseListTbl;
IMPORT TextWr;
IMPORT Wr, Thread;
IMPORT Fmt;
(* IMPORT Term; *)
<* FATAL Wr.Failure, Thread.Alerted *>
TYPE
  CharSearchArray = REF ARRAY CHAR OF TextSubsClauseList.T;
REVEAL
  T = Public BRANDED OBJECT
    tab: TextSubsClauseListTbl.T;
  OVERRIDES
    init := Init;
    add := Add;
    int := Int;
    apply := Apply;
  END;

PROCEDURE GetSearchChars(self: T): CharSearchArray =
  VAR
    result := NEW(CharSearchArray);
    iterate := self.tab.iterate();
    c: CHAR;
    l: TextSubsClauseList.T;
  BEGIN
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      result[i] := NIL;
    END;
    WHILE iterate.next(c, l) DO
      result[c] := l;
    END;
    RETURN result;
  END GetSearchChars;

PROCEDURE Init(self: T): T =
  BEGIN
    self.tab := NEW(TextSubsClauseListTbl.Default).init();
    RETURN self;
  END Init;

PROCEDURE Add(self: T; original, replacement: TEXT) =
  VAR
    c := Text.GetChar(original, 0);
    l: TextSubsClauseList.T := NIL;
    ts := TextSubsClause.T{original, replacement};
  BEGIN
    EVAL self.tab.get(c, l);
    l := TextSubsClauseList.Cons(ts, l);
    EVAL self.tab.put(c, l);
  END Add;

PROCEDURE Int(self: T; original: TEXT; replacement: INTEGER) =
  BEGIN
    self.add(original, Fmt.Int(replacement));
  END Int;

PROCEDURE Apply(self: T; src: TEXT): TEXT =
  VAR
    wr := TextWr.New();
    c: CHAR;
    ca := GetSearchChars(self);
    pos, lastFlushed: INTEGER := 0;
    cur: TextSubsClauseList.T;
    orig, actual: TEXT;
    textLen := Text.Length(src);
  PROCEDURE Flush() =
    BEGIN
      Wr.PutText(wr, Text.Sub(src, lastFlushed, pos - lastFlushed));
    END Flush;
  BEGIN
    WHILE pos < textLen DO
      c := Text.GetChar(src, pos);
      cur := ca[c];
      WHILE cur # NIL DO
        orig := cur.head.original;
        actual := Text.Sub(src, pos, Text.Length(orig));
        (* Term.WrLn("orig = \"" & orig & "\"\n");
           Term.WrLn("actual = \"" & actual & "\"\n"); *)
        IF Text.Equal(orig, actual) THEN
          (* Term.WrLn("replacing with " & cur.head.replacement & "\n"); *)
          Flush();
          Wr.PutText(wr, cur.head.replacement);
          pos := pos + Text.Length(orig);
          lastFlushed := pos;
          cur := NIL;
          DEC(pos);
        ELSE
          cur := cur.tail;
        END;
      END;
      INC(pos);
    END;
    Flush();
    RETURN TextWr.ToText(wr);
  END Apply;

BEGIN
END TextSubs.
    
