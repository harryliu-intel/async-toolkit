(* $Id$ *)

UNSAFE MODULE XMLParse;

(* this module is unsafe only because it uses M3toC *)

IMPORT xmlParser;
IMPORT Ctypes, M3toC, Debug, RefSeq, TextWr, Wr;
IMPORT Text;
IMPORT Pathname;

IMPORT Thread;
IMPORT RTCollector; (* Enable/Disable to avoid stepping on malloc *)
IMPORT SchedulerIndirection;  (* enable/disable to avoid malloc on malloc *)
IMPORT TextList;

CONST TE = Text.Equal;

PROCEDURE Start(stuff : REFANY; el : Ctypes.const_char_star) = 
  BEGIN
    IF Debug.GetLevel() > 10 THEN
      Debug.Out("Start: " & M3toC.CopyStoT(el),11)
    END;

    WITH ru = NARROW(stuff, REF U) DO
      IF ru^ # NIL THEN
        IF ru.ignore > 0 THEN
          INC(ru.ignore);
          RETURN
        ELSIF TextList.Member(avoidTags, ru.el) THEN
          INC(ru.ignore);
          RETURN
        END
      END;

      ru^ := NEW(U, 
                 el := M3toC.CopyStoT(el),
                 up := ru^, 
                 a := NEW(RefSeq.T).init(),
                 c := NEW(RefSeq.T).init(),
                 charWr := NEW(TextWr.T).init());

      IF ru.up # NIL THEN
        ru.up.c.addhi(ru^)
      END
    END;

  END Start;

PROCEDURE AttrP(stuff : REFANY; tag, attr : Ctypes.const_char_star) =
  BEGIN
    WITH u = NARROW(stuff, REF U)^ DO
      IF u.ignore > 0 THEN RETURN END;
      u.a.addhi(NEW(REF Attr, 
                    tag := M3toC.CopyStoT(tag), 
                    attr := M3toC.CopyStoT(attr)))
    END;
    Debug.Out("Attr: " & M3toC.CopyStoT(tag) & " = " & M3toC.CopyStoT(attr),11)
  END AttrP;

PROCEDURE End(stuff : REFANY) =
  BEGIN 
    (* fill in T fields *)
    IF Debug.GetLevel() > 10 THEN
      Debug.Out("End.",11)
    END;

    WITH u = NARROW(stuff, REF U)^ DO
      IF u.ignore > 0 THEN
        DEC(u.ignore); RETURN 
      END;

      u.attrs := NEW(REF ARRAY OF Attr, u.a.size());
      FOR i := 0 TO u.a.size()-1 DO
        u.attrs[i] := NARROW(u.a.get(i),REF Attr)^
      END;

      u.children := NEW(REF ARRAY OF T, u.c.size());
      FOR i := 0 TO u.c.size()-1 DO
        u.children[i] := u.c.get(i)
      END;

      u.charData := TextWr.ToText(u.charWr)
    END;

    WITH ru = NARROW(stuff, REF U) DO
      IF ru^.up # NIL THEN ru^ := ru^.up END
    END;

  END End;

PROCEDURE CharData(stuff : REFANY; len : CARDINAL; data : Ctypes.const_char_star) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  VAR
    cp : REF CHAR;
  BEGIN
    WITH u = NARROW(stuff, REF U)^ DO
      IF u.ignore > 0 THEN RETURN END;
      VAR x := LOOPHOLE(data,ADDRESS); BEGIN
        WHILE x < data + len DO
          cp := LOOPHOLE(x,REF CHAR);
          Wr.PutChar(u.charWr,cp^);
          INC(x)
        END
      END; 
      (*Wr.PutChar(u.charWr, '\000')*)
    END
  END CharData;

VAR
  avoidTags : TextList.T;

PROCEDURE DoIt(p : Pathname.T; avoidTagsArg : TextList.T) : T =
  VAR
    ru := NEW(REF U);
  BEGIN
    ru^ := NIL;
    avoidTags := avoidTagsArg;

    WITH s = M3toC.CopyTtoS(p) DO
      TRY
        RTCollector.Disable(); (* xmlParser uses malloc *)
        SchedulerIndirection.DisableSwitching();
        EVAL xmlParser.xmlParserMain(s, ru, Start, AttrP, End, CharData)
      FINALLY
        SchedulerIndirection.EnableSwitching();
        RTCollector.Enable();
        M3toC.FreeCopiedS(s)
      END
    END;

    RETURN ru^
  END DoIt;

PROCEDURE DoText(t : TEXT) : T =
  VAR
    ru := NEW(REF U);
  BEGIN
    ru^ := NIL;

    WITH s = M3toC.CopyTtoS(t) DO
      TRY
        RTCollector.Disable(); (* xmlParser uses malloc *)
        SchedulerIndirection.DisableSwitching();
        EVAL xmlParser.xmlParserString(s, ru, Start, AttrP, End, CharData)
      FINALLY
        SchedulerIndirection.EnableSwitching();
        RTCollector.Enable();
        M3toC.FreeCopiedS(s)
      END
    END;

    RETURN ru^
  END DoText;

REVEAL
  T = Public BRANDED Brand OBJECT
    el : TEXT;
    attrs : REF ARRAY OF Attr;
    children : REF ARRAY OF T;
    charData : TEXT;
  OVERRIDES 
    getAttr := GetAttr;
    getChild := GetChild;
    getEl := GetEl;
    getCharData := GetCharData;

    nChildren := NChildren;
    nAttrs := NAttrs;

    iterateChildren := IterateChildren;
    iterateAttrs := IterateAttrs;
  END;

  AttrIterator = PubAttrIterator BRANDED Brand & " Attr Iterator" OBJECT
    t : T;
    nidx : CARDINAL := 0;
  OVERRIDES
    next := AIN;
  END;

  ChildIterator = PubChildIterator BRANDED Brand & " Child Iterator" OBJECT
    t : T;
    nidx : CARDINAL := 0;
  OVERRIDES
    next := CIN;
  END;

PROCEDURE IterateChildren(t : T): ChildIterator = 
  BEGIN RETURN NEW(ChildIterator, t := t) END IterateChildren;

PROCEDURE IterateAttrs(t : T): AttrIterator = 
  BEGIN RETURN NEW(AttrIterator, t := t) END IterateAttrs;

PROCEDURE NChildren(t : T) : CARDINAL = 
  BEGIN RETURN NUMBER(t.children^) END NChildren;

PROCEDURE NAttrs(t : T) : CARDINAL = 
  BEGIN RETURN NUMBER(t.attrs^) END NAttrs;

PROCEDURE AIN(i : AttrIterator; VAR attr : Attr) : BOOLEAN =
  BEGIN
    IF i.nidx = NUMBER(i.t.attrs^) THEN 
      RETURN FALSE
    ELSE
      TRY
        attr := i.t.attrs[i.nidx];
        RETURN TRUE
      FINALLY
        INC(i.nidx)
      END
    END
  END AIN;

PROCEDURE CIN(i : ChildIterator; VAR child : T) : BOOLEAN =
  BEGIN
    IF i.nidx = NUMBER(i.t.children^) THEN 
      RETURN FALSE
    ELSE
      TRY
        child := i.t.children[i.nidx];
        RETURN TRUE
      FINALLY
        INC(i.nidx)
      END
    END
  END CIN;

PROCEDURE GetEl(t : T) : TEXT = BEGIN RETURN t.el END GetEl;

PROCEDURE GetCharData(t : T) : TEXT = BEGIN RETURN t.charData END GetCharData;

PROCEDURE GetAttr(t : T; n : TEXT) : Attr RAISES { NotFound, Multiple } =
  VAR
    res : Attr;
    found := FALSE;
  BEGIN
    FOR i := FIRST(t.attrs^) TO LAST(t.attrs^) DO
      IF TE(n,t.attrs[i].tag) THEN
        IF found THEN RAISE Multiple END;
        res := t.attrs[i]; found := TRUE
      END
    END;
    IF NOT found THEN RAISE NotFound END;
    RETURN res
  END GetAttr;

PROCEDURE GetChild(t : T; n : TEXT) : T RAISES { NotFound, Multiple } =
  VAR
    res : T;
    found := FALSE;
  BEGIN
    FOR i := FIRST(t.children^) TO LAST(t.children^) DO
      IF TE(n,t.children[i].el) THEN         
        IF found THEN RAISE Multiple END;
        res := t.children[i]; found := TRUE
      END
    END;
    IF NOT found THEN RAISE NotFound END;
    RETURN res
  END GetChild;

TYPE 
  U = T OBJECT
    up : U;
    a : RefSeq.T;
    c : RefSeq.T;
    charWr : TextWr.T;
    ignore : CARDINAL := 0;
  END;

BEGIN END XMLParse.
