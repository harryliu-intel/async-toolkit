MODULE Bnf;
IMPORT Word;
IMPORT Text;
IMPORT BnfSetList;
IMPORT BnfList;
IMPORT TextBnfSeq;
IMPORT BnfSeq;
IMPORT Debug;
IMPORT Wx;
IMPORT CharNames;
IMPORT TextBnf;
FROM Fmt IMPORT Int;

CONST TE = Text.Equal;

TYPE
  Array = REF ARRAY OF T;
  
REVEAL
  T = Public BRANDED Brand OBJECT
    hashV : Word.T;
  METHODS
    copy() : T;
    deepCopy() : T;
  OVERRIDES
    init := Init;
  END;

  Ident = PubIdent BRANDED Brand & " Ident" OBJECT
  OVERRIDES
    copy := CopyIdent;
    deepCopy := CopyIdent;
    equal := EqualIdent;
  END;
  
  String = PubString BRANDED Brand & " String" OBJECT
  OVERRIDES
    copy := CopyString;
    deepCopy := CopyString;
    equal := EqualString;
  END;
  
  ListOf = PubListOf BRANDED Brand & " ListOf" OBJECT
  OVERRIDES
    copy := CopyListOf;
    deepCopy := DeepCopyListOf;
    replaceChild := ReplaceChildListOf;
    equal := EqualListOf;
  END;
  
  Optional = PubOptional BRANDED Brand & " Optional" OBJECT
  OVERRIDES
    copy := CopyOptional;
    deepCopy := DeepCopyOptional;
    replaceChild := ReplaceChildOptional;
    equal := EqualOptional;
  END;
  
  Disjunction = PubDisjunction BRANDED Brand & " Disjunction" OBJECT
  OVERRIDES
    copy := CopyDisjunction;
    deepCopy := DeepCopyDisjunction;
    replaceChild := ReplaceChildDisjunction;
    equal := EqualDisjunction;
  END;
  
  Sequence = PubSequence BRANDED Brand & " Sequence" OBJECT
  OVERRIDES
    copy := CopySequence;
    deepCopy := DeepCopySequence;
    replaceChild := ReplaceChildSequence;
    equal := EqualSequence;
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

PROCEDURE EqualDisjunction(y : Disjunction; x : T) : BOOLEAN =
  BEGIN
    TYPECASE x OF
      Disjunction(d) =>
      VAR
        ye, de := NEW(BnfSetList.T).init();
      BEGIN
        FOR i := FIRST(y.elems^) TO LAST(y.elems^) DO
          EVAL ye.insert(y.elems[i])
        END;
        FOR i := FIRST(d.elems^) TO LAST(d.elems^) DO
          EVAL de.insert(d.elems[i])
        END;
        RETURN ye.equal(de)
      END
    ELSE
      RETURN FALSE
    END
  END EqualDisjunction;

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

PROCEDURE VisitPre(t : T; visitor : Visitor) =
  BEGIN
    visitor.visit(t);
    VisitChildren(t, visitor, VisitPre)
  END VisitPre;

PROCEDURE VisitPost(t : T; visitor : Visitor) =
  BEGIN
    VisitChildren(t, visitor, VisitPost);
    visitor.visit(t);
  END VisitPost;

TYPE Recurse = PROCEDURE (t : T; visitor : Visitor);
     
PROCEDURE VisitChildren(t : T; visitor : Visitor; recurse : Recurse) =
  BEGIN
    TYPECASE t OF
      ListOf(lst) => recurse(lst.elem, visitor)
    |
      Optional(opt) => recurse(opt.elem, visitor)
    |
      Disjunction(dis) =>
      FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
        recurse(dis.elems[i], visitor)
      END
    |
      Sequence(seq) =>
      FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
        recurse(seq.elems[i], visitor)
      END
    ELSE
      (* skip *)
    END
  END VisitChildren;

  (**********************************************************************)

PROCEDURE Distribute(parent, me : T) =
  BEGIN
    TYPECASE me OF
      ListOf(lst) =>
      IF ISTYPE(lst.elem, Disjunction) THEN
        VAR
          new : Disjunction := lst.elem.copy();
        BEGIN
          FOR i := FIRST(new.elems^) TO LAST(new.elems^) DO
            VAR newMe : ListOf := me.copy(); BEGIN
              newMe.elem := new.elems[i];
              new.elems[i] := newMe;
            END
          END;
          parent.replaceChild(me, new)
        END
      END
    |
      Optional(opt) =>
      IF ISTYPE(opt.elem, Disjunction) THEN
        VAR
          new : Disjunction := opt.elem.copy();
        BEGIN
          FOR i := FIRST(new.elems^) TO LAST(new.elems^) DO
            VAR newMe : Optional := me.copy(); BEGIN
              newMe.elem := new.elems[i];
              new.elems[i] := newMe;
            END
          END;
          parent.replaceChild(me, new)
        END
      END
    |
      Disjunction(dis) =>
      FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
        TYPECASE dis.elems[i] OF
          Disjunction(d) =>
          VAR
            n := NUMBER(dis.elems^); (* how many children do I have *)
            m := NUMBER(d.elems^);   (* how many children does child have *)
            new := NEW(Disjunction, elems := NEW(Array, n + m - 1));
          BEGIN
            SUBARRAY(new.elems^, 0, i) := SUBARRAY(dis.elems^, 0, i);
            SUBARRAY(new.elems^, i, m) := d.elems^;
            SUBARRAY(new.elems^, i + m, n - i - 1) :=
                SUBARRAY(dis.elems^, i + 1, n - i - 1);
            parent.replaceChild(me, new);
          END
        ELSE
          (* skip *)
        END
      END
    |
      Sequence(seq) =>
      FOR j := FIRST(seq.elems^) TO LAST(seq.elems^) DO
        IF ISTYPE(seq.elems[j], Disjunction) THEN
          VAR
            new : Disjunction := seq.elems[j].copy();
          BEGIN
            FOR i := FIRST(new.elems^) TO LAST(new.elems^) DO
              VAR newMe : Sequence := me.copy(); BEGIN
                newMe.elems[j] := new.elems[i];
                new.elems[i] := newMe;
              END
            END;
            parent.replaceChild(me, new)
          END
        END
      END
    ELSE
      (* skip *)
    END
  END Distribute;

PROCEDURE DistributeRec(p, m : T) =
  BEGIN
    TYPECASE m OF
      ListOf(lst) =>
      Distribute(p, m);
      DistributeRec(lst, lst.elem)
    |
      Optional(opt) =>
      Distribute(p, m);
      DistributeRec(opt, opt.elem)
    |
      Disjunction(dis) =>
      Distribute(p, m); 
      FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
        DistributeRec(dis, dis.elems[i])
      END
    |
      Sequence(seq) =>
      Distribute(p, m);
      FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
        DistributeRec(seq, seq.elems[i])
      END
    ELSE
      (* skip *)
    END
  END DistributeRec;

PROCEDURE DistributeAll(t : T;
                        <*UNUSED*>seq : REFANY (* TextBnfSeq.T *);
                        <*UNUSED*>stringMapper : StringMapper) : T =
  VAR elems := NEW(Array, 1);
      root := NEW(Disjunction, elems := elems);
  BEGIN
    elems[0] := t;
    DistributeRec(root, t);
    <*ASSERT NUMBER(root.elems^) = 1*>
    RETURN root.elems[0]
  END DistributeAll;

  (**********************************************************************)

TYPE Mapper = OBJECT METHODS f(t : T) : T END;
     
PROCEDURE MapRecursively(m : T; f : Mapper) : T =
  BEGIN
    TYPECASE m OF
      Ident(id) =>
      RETURN f.f(id)
    |
      String(str) =>
      RETURN f.f(str)
    |
      ListOf(lst) =>
      VAR
        elem := MapRecursively(lst.elem, f);
        res  := NEW(ListOf, elem := elem).init();
      BEGIN
        RETURN f.f(res)
      END
    |
      Optional(opt) =>
      VAR
        elem := MapRecursively(opt.elem, f);
        res  := NEW(Optional, elem := elem).init();
      BEGIN
        RETURN f.f(res)
      END
    |
      Disjunction(dis) =>
      VAR
        elems := NEW(Array, NUMBER(dis.elems^));
      BEGIN
        FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
          elems[i] := MapRecursively(dis.elems[i], f)
        END;
        RETURN f.f(NEW(Disjunction, elems := elems).init());
      END
    |
      Sequence(seq) =>
      VAR
        elems := NEW(Array, NUMBER(seq.elems^));
      BEGIN
        FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
          elems[i] := MapRecursively(seq.elems[i], f)
        END;
        RETURN f.f(NEW(Sequence, elems := elems).init());
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END MapRecursively;

  (**********************************************************************)

PROCEDURE RemoveIdentListsM(m : EditObj; t : T) : T =
  BEGIN
    TYPECASE t OF
      ListOf(lst) =>
        (* remove the pattern { X } *)
        TYPECASE lst.elem OF
          Ident(e) =>
          WITH nm = "list1of_" & e.ident,
               ident = MakeIdent(nm) DO
            
            (* need to add the new rule here *)
            WITH empty = MakeSequence(ARRAY OF T {}),
                 list  = MakeSequence(ARRAY OF T { lst.elem, ident }),
                 dis   = MakeDisjunction(ARRAY OF T { empty, list }) DO
              m.seq.addhi(TextBnf.T { nm, dis });
              RETURN ident
            END
          END
        ELSE
        END
    ELSE
    END;
    RETURN t
  END RemoveIdentListsM;
  
PROCEDURE RemoveIdentLists(t    : T;
                           seqA : REFANY (* TextBnfSeq.T *);
                           stringMapper : StringMapper) : T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveIdentListsM);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveIdentLists;

  (**********************************************************************)

PROCEDURE RemoveOSI(m : EditObj; t : T) : T =
  BEGIN
    TYPECASE t OF
      Optional(opt) =>
        (* remove the patterns  [X] ["S"]  *)
        TYPECASE opt.elem OF
          Ident(e) =>
          WITH nm = "opt_" & e.ident,
               ident = MakeIdent(nm),
               empty = MakeSequence(ARRAY OF T {}),
               rule = MakeDisjunction(ARRAY OF T { empty, opt.elem }) DO
            m.seq.addhi(TextBnf.T { nm, rule });
            RETURN ident
          END
        |
          String(s) =>
          WITH nm = "opt_" & m.stringMapper(s.string),
               ident = MakeIdent(nm),
               empty = MakeSequence(ARRAY OF T {}),
               rule = MakeDisjunction(ARRAY OF T { empty, opt.elem }) DO
            m.seq.addhi(TextBnf.T { nm, rule });
            RETURN ident
          END
        ELSE
        END
    ELSE
    END;
    RETURN t
  END RemoveOSI;
  
PROCEDURE RemoveOptionalStringIdent(t    : T;
                                    seqA : REFANY (* TextBnfSeq.T *);
                                    stringMapper : StringMapper) : T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveOSI);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveOptionalStringIdent;

  (**********************************************************************)
  
TYPE
  EditObj = Mapper OBJECT
    seq          : TextBnfSeq.T;
    stringMapper : StringMapper;
  END;

PROCEDURE RemoveListsFromSeq(m : EditObj; t : T) : T =
  BEGIN
    TYPECASE t OF
      Sequence(seq) =>
      VAR
        new := NEW(BnfSeq.T).init();
        i := 0;
      BEGIN
        WHILE i < NUMBER(seq.elems^) DO
          IF i # LAST(seq.elems^) THEN
            WITH match = MatchListPat(m,
                                      seq.elems[i],
                                      seq.elems[i+1]) DO
              IF match # NIL THEN
                new.addhi(match);
                INC(i)
              ELSE
                new.addhi(seq.elems[i])
              END
            END
          ELSE
            new.addhi(seq.elems[i])
          END;
          INC(i)
        END;
        RETURN NEW(Sequence, elems := Seq2Array(new)).init()
      END
    ELSE
      RETURN t
    END
  END RemoveListsFromSeq;

  (**********************************************************************)

PROCEDURE RemoveNestedSeqs(<*UNUSED*>m : EditObj; t : T) : T =
  BEGIN
    TYPECASE t OF
      Sequence(seq) =>
      VAR
        elems := NEW(BnfSeq.T).init();
      BEGIN
        (* remove the pattern (... , ( ... , ...) ...)    *)
        FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
          TYPECASE seq.elems[i] OF
            Sequence(e) =>
            FOR j := FIRST(e.elems^) TO LAST(e.elems^) DO
              elems.addhi(e.elems[j])
            END
          ELSE
            elems.addhi(seq.elems[i])
          END
        END;
        RETURN NEW(Sequence, elems := Seq2Array(elems)).init()
      END
    ELSE
      RETURN t
    END;
  END RemoveNestedSeqs;
  
PROCEDURE RemoveNestedSequences(t    : T;
                                    seqA : REFANY (* TextBnfSeq.T *);
                                    stringMapper : StringMapper) : T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveNestedSeqs);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveNestedSequences;

  (**********************************************************************)

PROCEDURE RemoveSingletonSeqs(<*UNUSED*>m : EditObj; t : T) : T =
  BEGIN
    TYPECASE t OF
      Sequence(seq) =>
      IF NUMBER(seq.elems^) = 1 THEN RETURN seq.elems[0] END
    ELSE
    END;
    RETURN t
  END RemoveSingletonSeqs;
  
PROCEDURE RemoveSingletonSequences(t    : T;
                                    seqA : REFANY (* TextBnfSeq.T *);
                                    stringMapper : StringMapper) : T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveSingletonSeqs);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveSingletonSequences;

  (**********************************************************************)

TYPE
  RemoveOptionalsObj = EditObj OBJECT
    cnt : CARDINAL;
    nm  : TEXT;
  END;
  
PROCEDURE RemoveRemOptionals(m : RemoveOptionalsObj; t : T) : T =
  BEGIN
    TYPECASE t OF
      Optional(opt) =>
      WITH nm    =  "opt_" & m.nm & "_" & Int(m.cnt),
           ident = MakeIdent(nm),
           empty = MakeSequence(ARRAY OF T {}),
           rule  = MakeDisjunction(ARRAY OF T { opt.elem, empty }) DO
        INC(m.cnt);
        (* need to make new rule here *)
        m.seq.addhi(TextBnf.T { nm, rule });
        RETURN ident
      END
    ELSE
    END;
    RETURN t
  END RemoveRemOptionals;
  
PROCEDURE RemoveRemainingOptionals(t    : T;
                                   seqA : REFANY (* TextBnfSeq.T *);
                                   stringMapper : StringMapper) : T =
  VAR
    seq : TextBnfSeq.T := seqA;
    m : RemoveOptionalsObj;
    nm : TEXT := NIL;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH rec = seq.get(i) DO
        IF rec.b = t THEN
          nm := rec.t
        END
      END
    END;

    IF nm = NIL THEN
      Debug.Error("RemoveRemainingOptionals : no name for production " &
        DebugBnf(t, 0)
      )
    END;
    
    m := NEW(RemoveOptionalsObj,
             cnt := 0,
             nm := nm,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveRemOptionals);

    RETURN MapRecursively(t, m)
  END RemoveRemainingOptionals;

  (**********************************************************************)
  
PROCEDURE MatchListPat(m : EditObj; s0, s1 : T) : T =
  BEGIN
    (* this nasty bit of code looks for the patterns
       
       X { X }
       
       and
       
       X { string X }
    *)
          
    TYPECASE s0 OF
      Ident(e) => (* X ... *)
      TYPECASE s1 OF
        ListOf (ll) =>

        (* X { ... } *)
        
        IF  ll.elem = e THEN
          (* X { X } *)
          WITH nm    = "list1of_" & e.ident,
               ident = MakeIdent(nm) DO

            (* add list rule here *)
            
            WITH single = ll.elem,
                 list   = MakeSequence   (ARRAY OF T { single, ident }),
                 dis    = MakeDisjunction(ARRAY OF T { single, list }) DO
              m.seq.addhi(TextBnf.T { nm, dis })
            END;
              
            RETURN ident
          END         
              
        ELSIF ISTYPE(ll.elem, Sequence) THEN
          WITH iseq = NARROW(ll.elem, Sequence) DO
            IF NUMBER(iseq.elems^) = 2 AND
              ISTYPE(iseq.elems[0], String) AND
              e = iseq.elems[1] THEN
              
              (* X { <string> X } *)

              WITH nm = "list1of_" &
                   m.stringMapper(NARROW(iseq.elems[0],String).string) &
                   "_" &
                   e.ident,
                   ident = MakeIdent(nm) DO
                WITH single = ll.elem,
                     delim  = iseq.elems[0],
                     list = MakeSequence(ARRAY OF T { delim, single, ident }),
                     dis  = MakeDisjunction(ARRAY OF T { single, list }) DO
                  m.seq.addhi(TextBnf.T { nm, dis });
                  RETURN ident
                END              
              END
            END
          END
        END
      ELSE (* skip *)
      END
    ELSE (* skip *)
    END;
    RETURN NIL
  END MatchListPat;

PROCEDURE Seq2Array(seq : BnfSeq.T) : Array =
  VAR
    res := NEW(Array, seq.size());
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := seq.get(i)
    END;
    RETURN res
  END Seq2Array;
  
PROCEDURE RemoveSeqLists(t    : T;
                         seqA : REFANY (* TextBnfSeq.T *);
                         stringMapper : StringMapper) : T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveListsFromSeq);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveSeqLists;
  
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
