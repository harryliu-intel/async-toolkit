(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Bnf;

IMPORT BnfClass;

IMPORT Word;
IMPORT Text;
IMPORT BnfSet;
IMPORT BnfSetList;
IMPORT BnfList;
IMPORT Wx;
IMPORT CharNames;

CONST TE = Text.Equal;

REVEAL
  T = BnfClass.Private BRANDED Brand OBJECT
    hashV : Word.T;
  OVERRIDES
    init := Init;
  END;

  Ident = PubIdent BRANDED Brand & " Ident" OBJECT
  OVERRIDES
    copy     := CopyIdent;
    deepCopy := CopyIdent;
    equal    := EqualIdent;
  END;
  
  String = PubString BRANDED Brand & " String" OBJECT
  OVERRIDES
    copy     := CopyString;
    deepCopy := CopyString;
    equal    := EqualString;
  END;
  
  ListOf = PubListOf BRANDED Brand & " ListOf" OBJECT
  OVERRIDES
    copy         := CopyListOf;
    deepCopy     := DeepCopyListOf;
    replaceChild := ReplaceChildListOf;
    equal        := EqualListOf;
  END;
  
  Optional = PubOptional BRANDED Brand & " Optional" OBJECT
  OVERRIDES
    copy         := CopyOptional;
    deepCopy     := DeepCopyOptional;
    replaceChild := ReplaceChildOptional;
    equal        := EqualOptional;
  END;
  
  Disjunction = PubDisjunction BRANDED Brand & " Disjunction" OBJECT
  OVERRIDES
    copy         := CopyDisjunction;
    deepCopy     := DeepCopyDisjunction;
    replaceChild := ReplaceChildDisjunction;
    equal        := EqualDisjunction;
    init         := InitDisjunction;
  END;
  
  Sequence = PubSequence BRANDED Brand & " Sequence" OBJECT
  OVERRIDES
    copy         := CopySequence;
    deepCopy     := DeepCopySequence;
    replaceChild := ReplaceChildSequence;
    equal        := EqualSequence;
  END;

VAR set : BnfList.T := NIL;
  
PROCEDURE Init(t : T) : T =
  VAR
    p := set;
  BEGIN
    WHILE p # NIL DO
      IF Equal(t, p.head) THEN
        RETURN p.head
      END;
      p := p.tail
    END;
    set := BnfList.Cons(t, set);
    RETURN t
  END Init;

  (**********************************************************************)

PROCEDURE DebugFmt(t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutChar(wx, '(');
    TYPECASE t OF
      Ident(id) =>
      Wx.PutText(wx, "*ident* ");
      Wx.PutText(wx, id.ident)
    |
      String(str) =>
      Wx.PutText(wx, "*string* ");
      Wx.PutText(wx, str.string)
    |
      ListOf(lst) =>
      Wx.PutText(wx, "*listof* ");
      Wx.PutText(wx, DebugFmt(lst.elem))
    |
      Optional(opt) =>
      Wx.PutText(wx, "*optional* ");
      Wx.PutText(wx, DebugFmt(opt.elem))
    |
      Disjunction(dis) =>
      Wx.PutText(wx, "*disjunction*");
      FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
        Wx.PutChar(wx, ' ');
        Wx.PutText(wx, DebugFmt(dis.elems[i]))
      END
    |
      Sequence(seq) =>
      Wx.PutText(wx, "*sequence*");
      FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
        Wx.PutChar(wx, ' ');
        Wx.PutText(wx, DebugFmt(seq.elems[i]))
      END
    ELSE
      <*ASSERT FALSE*>
    END;
    Wx.PutChar(wx, ')');
    RETURN Wx.ToText(wx)
  END DebugFmt;

PROCEDURE CopyIdent(t : T) : T = BEGIN RETURN t END CopyIdent;

CONST CopyString = CopyIdent;

PROCEDURE EqualString(y : String; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      String(s) => RETURN TE(y.string, s.string)
    ELSE
      RETURN FALSE
    END
  END EqualString;

PROCEDURE EqualIdent(y : Ident; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      Ident(id) => RETURN TE(y.ident, id.ident)
    ELSE
      RETURN FALSE
    END
  END EqualIdent;

  (**********************************************************************)

PROCEDURE CopyListOf(x : ListOf) : T =
  BEGIN
    RETURN NEW(ListOf, elem := x.elem)
  END CopyListOf;

PROCEDURE DeepCopyListOf(x : ListOf) : T =
  BEGIN
    RETURN NEW(ListOf, elem := x.elem.deepCopy())
  END DeepCopyListOf;

PROCEDURE ReplaceChildListOf(x : ListOf; o, n : T) =
  BEGIN
    <*ASSERT x.elem = o*>
    x.elem := n
  END ReplaceChildListOf;

PROCEDURE EqualListOf(y : ListOf; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      ListOf(lo) => RETURN lo.elem.equal(y.elem)
    ELSE
      RETURN FALSE
    END
  END EqualListOf;

  (**********************************************************************)

PROCEDURE CopyOptional(x : Optional) : T =
  BEGIN
    RETURN NEW(Optional, elem := x.elem)
  END CopyOptional;

PROCEDURE DeepCopyOptional(x : Optional) : T =
  BEGIN
    RETURN NEW(Optional, elem := x.elem.deepCopy())
  END DeepCopyOptional;

PROCEDURE ReplaceChildOptional(x : Optional; o, n : T) =
  BEGIN
    <*ASSERT x.elem = o*>
    x.elem := n
  END ReplaceChildOptional;

PROCEDURE EqualOptional(y : Optional; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      Optional(lo) => RETURN lo.elem.equal(y.elem)
    ELSE
      RETURN FALSE
    END
  END EqualOptional;

  (**********************************************************************)

PROCEDURE CopyDisjunction(x : Disjunction) : T =
  BEGIN
    WITH elems = NEW(Array, NUMBER(x.elems^)) DO
      elems^ := x.elems^;
      RETURN NEW(Disjunction, elems := elems)
    END
  END CopyDisjunction;

PROCEDURE DeepCopyDisjunction(x : Disjunction) : T =
  BEGIN
    WITH elems = NEW(Array, NUMBER(x.elems^)) DO
      FOR i := FIRST(elems^) TO LAST(elems^) DO
        elems[i] := x.elems[i].deepCopy()
      END;
      RETURN NEW(Disjunction, elems := elems)
    END
  END DeepCopyDisjunction;

PROCEDURE ReplaceChildDisjunction(x : Disjunction; o, n : T) =
  BEGIN
    FOR i := FIRST(x.elems^) TO LAST(x.elems^) DO
      IF x.elems[i] = o THEN
        x.elems[i] := n;
        RETURN
      END
    END;
    <*ASSERT FALSE*>
  END ReplaceChildDisjunction;

PROCEDURE DisjunctionSet(y : Disjunction) : BnfSet.T =
  VAR
    res := NEW(BnfSetList.T).init();
  BEGIN
    FOR i := FIRST(y.elems^) TO LAST(y.elems^) DO
      EVAL res.insert(y.elems[i])
    END;
    RETURN res
  END DisjunctionSet;
  
PROCEDURE EqualDisjunction(y : Disjunction; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      Disjunction(d) =>
      VAR
        ye := DisjunctionSet(y);
        de := DisjunctionSet(d);
      BEGIN
        RETURN ye.equal(de)
      END
    ELSE
      RETURN FALSE
    END
  END EqualDisjunction;

PROCEDURE InitDisjunction(x : Disjunction) : T =
  VAR
    set := DisjunctionSet(x);
    n := set.size();
  BEGIN
    IF n < NUMBER(x.elems^) THEN
      x.elems := NEW(REF ARRAY OF T, n);
      VAR iter := set.iterate();
      BEGIN
        FOR i := FIRST(x.elems^) TO LAST(x.elems^) DO
          EVAL iter.next(x.elems[i])
        END
      END
    END;
    RETURN T.init(x)
  END InitDisjunction;

  (**********************************************************************)

PROCEDURE CopySequence(x : Sequence) : T =
  BEGIN
    WITH elems = NEW(Array, NUMBER(x.elems^)) DO
      elems^ := x.elems^;
      RETURN NEW(Sequence, elems := elems)
    END
  END CopySequence;

PROCEDURE DeepCopySequence(x : Sequence) : T =
  BEGIN
    WITH elems = NEW(Array, NUMBER(x.elems^)) DO
      FOR i := FIRST(elems^) TO LAST(elems^) DO
        elems[i] := x.elems[i].deepCopy()
      END;
      RETURN NEW(Disjunction, elems := elems)
    END
  END DeepCopySequence;

PROCEDURE ReplaceChildSequence(x : Sequence; o, n : T) =
  BEGIN
    FOR i := FIRST(x.elems^) TO LAST(x.elems^) DO
      IF x.elems[i] = o THEN
        x.elems[i] := n;
        RETURN
      END
    END;
    <*ASSERT FALSE*>
  END ReplaceChildSequence;

PROCEDURE EqualSequence(y : Sequence; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      Sequence(seq) =>
      IF NUMBER(y.elems^) # NUMBER(seq.elems^) THEN
        RETURN FALSE
      ELSE
        FOR i := FIRST(y.elems^) TO LAST(y.elems^) DO
          IF NOT y.elems[i].equal(seq.elems[i]) THEN RETURN FALSE END
        END;
        RETURN TRUE
      END
    ELSE
      RETURN FALSE
    END
  END EqualSequence;

  (**********************************************************************)

  (**********************************************************************)


PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN RETURN a.equal(b) END Equal;
  
PROCEDURE DebugBnf(x : T; lev : CARDINAL) : TEXT =

  PROCEDURE Lev() : TEXT =
    VAR
      z := NEW(REF ARRAY OF CHAR, 4*(lev+1));
    BEGIN
      FOR i := FIRST(z^) TO LAST(z^) DO
        z[i] :=  ' '
      END;
      RETURN Text.FromChars(z^)
    END Lev;

  VAR
    nxt := lev + 1;
  BEGIN
    TYPECASE x OF
      String(str) => RETURN "(*token* " & MapString(str.string) & ")"
    |
      Sequence(seq) =>
      VAR
        wx := Wx.New();
      BEGIN
        Wx.PutText(wx, "(*sequence* ");
        FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
          Wx.PutChar(wx, '\n');
          Wx.PutText(wx, Lev() & DebugBnf(seq.elems[i], nxt));
        END;
        Wx.PutText(wx, "\n" & Lev() & ")");
        RETURN Wx.ToText(wx)
      END
    |
      Ident(id) => RETURN "(*ident* " & id.ident & ")"
    |
      Optional(opt) =>
      RETURN "(*optional* " & DebugBnf(opt.elem, nxt) & ")"
    |
      ListOf(listof) =>
      RETURN "(*listof* " & DebugBnf(listof.elem, nxt) & ")"
    |
      Disjunction(dis) =>
      VAR
        wx := Wx.New();
      BEGIN
        Wx.PutText(wx, "(*disjunction* ");
        FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
          Wx.PutChar(wx, '\n');
          Wx.PutText(wx, Lev() & DebugBnf(dis.elems[i], nxt));
        END;
        Wx.PutText(wx, "\n" & Lev() & ")");
        RETURN Wx.ToText(wx)
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END DebugBnf;

PROCEDURE MapString(str : TEXT) : TEXT =
  (* this one is just for debugging.  It NEED NOT match the one used to
     produce the output grammar, at least for now *)
  VAR
    in := Text.Length(str);
    wx := Wx.New();
    to : TEXT;
  BEGIN
    FOR i := 1 TO in - 2 DO
      WITH c = Text.GetChar(str, i) DO
        IF CharNames.Map(c, to) THEN
          Wx.PutText(wx, to)
        ELSE
          Wx.PutChar(wx, c)
        END
      END
    END;
    RETURN Wx.ToText(wx)
  END MapString;

PROCEDURE MakeSequence(READONLY of : ARRAY OF T) : Sequence =
  VAR
    elems := NEW(REF ARRAY OF T, NUMBER(of));
  BEGIN 
    elems^ := of;
    RETURN NEW(Sequence, elems := elems).init()
  END MakeSequence;

PROCEDURE MakeDisjunction(READONLY of : ARRAY OF T) : Disjunction =
  VAR
    elems := NEW(REF ARRAY OF T, NUMBER(of));
  BEGIN
    elems^ := of;
    RETURN NEW(Disjunction, elems := elems).init()
  END MakeDisjunction;

PROCEDURE MakeOptional(of : T) : Optional =
  BEGIN RETURN NEW(Optional, elem := of).init() END MakeOptional;

PROCEDURE MakeListOf(of : T) : ListOf =
  BEGIN RETURN NEW(ListOf, elem := of).init() END MakeListOf;

PROCEDURE MakeIdent(nm : TEXT) : Ident =
  BEGIN RETURN NEW(Ident, ident := nm).init() END MakeIdent;

PROCEDURE MakeString(str : TEXT) : String =
  BEGIN RETURN NEW(String, string := str).init() END MakeString;

BEGIN END Bnf.
