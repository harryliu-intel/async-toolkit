(* $Id$ *)

UNSAFE MODULE XMLParse;

(* this module is unsafe only because it uses M3toC *)

IMPORT xmlParser;
IMPORT Ctypes, M3toC, Debug, RefSeq, TextWr, Wr;
IMPORT Text;
IMPORT Pathname;
IMPORT TextF, Cstring; (* this isn't REALLY what we want, to be honest *)

IMPORT Thread;

CONST TE = Text.Equal;

PROCEDURE Start(stuff : REFANY; el : Ctypes.const_char_star) = 
  BEGIN
    WITH ru = NARROW(stuff, REF U) DO
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

    Debug.Out("Start: " & M3toC.CopyStoT(el),11)
  END Start;

PROCEDURE AttrP(stuff : REFANY; tag, attr : Ctypes.const_char_star) =
  BEGIN
    WITH u = NARROW(stuff, REF U)^ DO
      u.a.addhi(NEW(REF Attr, 
                    tag := M3toC.CopyStoT(tag), 
                    attr := M3toC.CopyStoT(attr)))
    END;
    Debug.Out("Attr: " & M3toC.CopyStoT(tag) & " = " & M3toC.CopyStoT(attr),11)
  END AttrP;

PROCEDURE End(stuff : REFANY) =
  BEGIN 
    (* fill in T fields *)
    WITH u = NARROW(stuff, REF U)^ DO

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

    Debug.Out("End.",11)
  END End;

PROCEDURE CopyCStoT (s: Ctypes.const_char_star; len : INTEGER): TEXT =
  VAR t := NEW (TEXT, len+1);
  BEGIN
    EVAL Cstring.memcpy (ADR (t[0]), s, len);
    t[len] := '\000';
    RETURN t;
  END CopyCStoT;

PROCEDURE CharData(stuff : REFANY; len : CARDINAL; data : Ctypes.const_char_star) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    WITH u = NARROW(stuff, REF U)^,
         t = CopyCStoT(data,len) DO
      Wr.PutText(u.charWr,t)
    END
  END CharData;

PROCEDURE DoIt(p : Pathname.T) : T =
  VAR
    ru := NEW(REF U);
  BEGIN
    ru^ := NIL;

    EVAL xmlParser.xmlParserMain(M3toC.TtoS(p),
             ru, Start, AttrP, End, CharData);

    RETURN ru^
  END DoIt;

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
  END;

BEGIN END XMLParse.
