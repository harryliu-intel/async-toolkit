MODULE BnfEdit;
IMPORT Bnf;
IMPORT BnfClass;
IMPORT BnfSeq;
IMPORT BnfRuleSeq;
FROM Bnf IMPORT Array;
FROM Bnf IMPORT ListOf, Optional, Disjunction, Sequence, Ident, String;
FROM Bnf IMPORT MakeIdent, MakeDisjunction, MakeSequence;
FROM Bnf IMPORT DebugBnf, DebugFmt, DisjunctionSet;
IMPORT Text;
IMPORT BnfRule;
IMPORT Debug;
FROM Fmt IMPORT Int;

CONST TE = Text.Equal;

PROCEDURE Distribute(parent, me : Bnf.T) =
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

PROCEDURE DistributeRec(p, m : Bnf.T) =
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

PROCEDURE DistributeAll(t : Bnf.T;
                        <*UNUSED*>seq : BnfRuleSeq.T;
                        <*UNUSED*>stringMapper : StringMapper) : Bnf.T =
  VAR elems := NEW(Array, 1);
      root := NEW(Disjunction, elems := elems);
  BEGIN
    elems[0] := t;
    DistributeRec(root, t);
    <*ASSERT NUMBER(root.elems^) = 1*>
    RETURN root.elems[0]
  END DistributeAll;

  (**********************************************************************)

TYPE
  Mapper = OBJECT
    changed := FALSE;
  METHODS
    f(t : Bnf.T) : Bnf.T
  END;
     
PROCEDURE MapRecursively(m : Bnf.T; f : Mapper) : Bnf.T =
  VAR
    new : Bnf.T;
  BEGIN
    TYPECASE m OF
      Ident(id) =>
      new := f.f(id)
    |
      String(str) =>
      new := f.f(str)
    |
      ListOf(lst) =>
      VAR
        elem := MapRecursively(lst.elem, f);
        res  := NEW(ListOf, elem := elem).init();
      BEGIN
        new := f.f(res)
      END
    |
      Optional(opt) =>
      VAR
        elem := MapRecursively(opt.elem, f);
        res  := NEW(Optional, elem := elem).init();
      BEGIN
        new := f.f(res)
      END
    |
      Disjunction(dis) =>
      VAR
        elems := NEW(Array, NUMBER(dis.elems^));
      BEGIN
        FOR i := FIRST(dis.elems^) TO LAST(dis.elems^) DO
          elems[i] := MapRecursively(dis.elems[i], f)
        END;
        new := f.f(NEW(Disjunction, elems := elems).init());
      END
    |
      Sequence(seq) =>
      VAR
        elems := NEW(Array, NUMBER(seq.elems^));
      BEGIN
        FOR i := FIRST(seq.elems^) TO LAST(seq.elems^) DO
          elems[i] := MapRecursively(seq.elems[i], f)
        END;
        new := f.f(NEW(Sequence, elems := elems).init());
      END
    ELSE
      <*ASSERT FALSE*>
    END;
    f.changed := f.changed OR new # m;
    RETURN new
  END MapRecursively;

  (**********************************************************************)

PROCEDURE AttemptAdd(seq  : BnfRuleSeq.T;
                     nm   : TEXT;
                     expr : Bnf.T) : BOOLEAN =
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH old = seq.get(i) DO
        IF TE(nm, old.t) THEN
          IF old.b = expr THEN
            RETURN TRUE
          ELSE
            RETURN FALSE
          END
        END
      END
    END;
    seq.addhi(BnfRule.T { nm, expr, NIL });
    RETURN TRUE
  END AttemptAdd;
  
PROCEDURE RemoveIdentListsM(m : EditObj; t : Bnf.T) : Bnf.T =
  BEGIN
    TYPECASE t OF
      ListOf(lst) =>
        (* remove the pattern { X } *)
        TYPECASE lst.elem OF
          Ident(e) =>
          WITH nm = "listof_" & e.ident,
               ident = MakeIdent(nm),
               empty = MakeSequence(ARRAY OF Bnf.T {}),
               list  = MakeSequence(ARRAY OF Bnf.T { lst.elem, ident }),
               dis   = MakeDisjunction(ARRAY OF Bnf.T { empty, list }),
               success = AttemptAdd(m.seq, nm, dis) DO
            IF NOT success THEN
              Debug.Error("Multiple definitions for " & nm)
            END;
            <*ASSERT success*>
            RETURN ident
          END
        ELSE
        END
    ELSE
    END;
    RETURN t
  END RemoveIdentListsM;
  
PROCEDURE RemoveIdentLists(t    : Bnf.T;
                           seqA : BnfRuleSeq.T;
                           stringMapper : StringMapper) : Bnf.T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveIdentListsM);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveIdentLists;

  (**********************************************************************)

PROCEDURE RemoveOSI(m : EditObj; t : Bnf.T) : Bnf.T =
  BEGIN
    TYPECASE t OF
      Optional(opt) =>
        (* remove the patterns  [X] ["S"]  *)
        TYPECASE opt.elem OF
          Ident(e) =>
          WITH nm = "opt_" & e.ident,
               ident = MakeIdent(nm),
               empty = MakeSequence(ARRAY OF Bnf.T {}),
               rule = MakeDisjunction(ARRAY OF Bnf.T { empty, opt.elem }),
               success = AttemptAdd(m.seq, nm, rule) DO
            <*ASSERT success*>
            RETURN ident
          END
        |
          String(s) =>
          WITH nm      = "opt_" & m.stringMapper(s.string),
               ident   = MakeIdent(nm),
               empty   = MakeSequence(ARRAY OF Bnf.T {}),
               rule    = MakeDisjunction(ARRAY OF Bnf.T { empty, opt.elem }),
               success = AttemptAdd(m.seq, nm, rule) DO
            <*ASSERT success*>
            RETURN ident
          END
        ELSE
        END
    ELSE
    END;
    RETURN t
  END RemoveOSI;
  
PROCEDURE RemoveOptionalStringIdent(t    : Bnf.T;
                                    seqA : BnfRuleSeq.T;
                                    stringMapper : StringMapper) : Bnf.T =
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
    seq          : BnfRuleSeq.T;
    stringMapper : StringMapper;
  END;

PROCEDURE RemoveListsFromSeq(m : EditObj; t : Bnf.T) : Bnf.T =
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

PROCEDURE RemoveNestedSeqs(<*UNUSED*>m : EditObj; t : Bnf.T) : Bnf.T =
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
  
PROCEDURE RemoveNestedSequences(t    : Bnf.T;
                                    seqA : BnfRuleSeq.T;
                                    stringMapper : StringMapper) : Bnf.T =
  VAR
    m := NEW(EditObj,
             seq := seqA,
             stringMapper := stringMapper,
             f := RemoveNestedSeqs);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveNestedSequences;

  (**********************************************************************)

PROCEDURE RemoveSingletonSeqs(<*UNUSED*>m : EditObj; t : Bnf.T) : Bnf.T =
  BEGIN
    TYPECASE t OF
      Sequence(seq) =>
      IF NUMBER(seq.elems^) = 1 THEN RETURN seq.elems[0] END
    ELSE
    END;
    RETURN t
  END RemoveSingletonSeqs;
  
PROCEDURE RemoveSingletonSequences(t    : Bnf.T;
                                    seqA : BnfRuleSeq.T;
                                    stringMapper : StringMapper) : Bnf.T =
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
  
PROCEDURE RemoveRemOptionals(m : RemoveOptionalsObj; t : Bnf.T) : Bnf.T =
  BEGIN
    TYPECASE t OF
      Optional(opt) =>
      WITH nm    =  "opt_" & m.nm & "_" & Int(m.cnt),
           ident = MakeIdent(nm),
           empty = MakeSequence(ARRAY OF Bnf.T {}),
           rule  = MakeDisjunction(ARRAY OF Bnf.T { opt.elem, empty }),
           success = AttemptAdd(m.seq, nm, rule) DO
        <*ASSERT success*>
        INC(m.cnt);
        RETURN ident
      END
    ELSE
    END;
    RETURN t
  END RemoveRemOptionals;
  
PROCEDURE RemoveRemainingOptionals(t    : Bnf.T;
                                   seqA : BnfRuleSeq.T;
                                   stringMapper : StringMapper) : Bnf.T =
  VAR
    seq : BnfRuleSeq.T := seqA;
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

TYPE
  SubstituteObj = Mapper OBJECT
    from, to : Bnf.T;
  OVERRIDES
    f := SubstituteM;
  END;

PROCEDURE SubstituteM(m : SubstituteObj; t : Bnf.T) : Bnf.T =
  BEGIN
    IF m.from = t THEN
      RETURN m.to
    ELSE
      RETURN t
    END
  END SubstituteM;

PROCEDURE Substitute(t, from, to : Bnf.T) : Bnf.T =
  VAR
    m := NEW(SubstituteObj, from := from, to := to);
  BEGIN
    RETURN MapRecursively(t, m)
  END Substitute;
  
  (**********************************************************************)
  
PROCEDURE MatchListPat(m : EditObj; s0, s1 : Bnf.T) : Bnf.T =
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
          WITH nm      = "list1of_" & e.ident,
               single  = ll.elem,
               ident   = MakeIdent(nm),
               list    = MakeSequence   (ARRAY OF Bnf.T { single, ident }),
               dis     = MakeDisjunction(ARRAY OF Bnf.T { single, list }),
               success = AttemptAdd(m.seq, nm, dis) DO
            <*ASSERT success*>
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
                   single  = ll.elem,
                   delim   = iseq.elems[0],
                   ident   = MakeIdent(nm),
                   list    = MakeSequence(ARRAY OF Bnf.T { delim, single, ident }),
                   dis     = MakeDisjunction(ARRAY OF Bnf.T { single, list }),
                   success = AttemptAdd(m.seq, nm, dis) DO
                <*ASSERT success*>
                RETURN ident
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
  
PROCEDURE RemoveSeqLists(t    : Bnf.T;
                         seqA : BnfRuleSeq.T;
                         stringMapper : StringMapper) : Bnf.T =
  VAR
    m := NEW(EditObj,
             seq          := seqA,
             stringMapper := stringMapper,
             f            := RemoveListsFromSeq);
  BEGIN
    RETURN MapRecursively(t, m)
  END RemoveSeqLists;
  
  (**********************************************************************)

PROCEDURE Unify(a, b : Disjunction) : Disjunction =
  VAR
    setA  := DisjunctionSet(a);
    setB  := DisjunctionSet(b);
    union := setA.union(setB);
    iter  := union.iterate();
    res   := NEW(REF ARRAY OF Bnf.T, union.size());
  BEGIN
    Debug.Out("Unifying a : " & DebugFmt(a));
    Debug.Out("Unifying b : " & DebugFmt(b));
    FOR i := FIRST(res^) TO LAST(res^) DO
      WITH hadIt = iter.next(res[i]) DO
        <*ASSERT hadIt*>
      END
    END;
    
    WITH new = NEW(Disjunction, elems := res).init() DO
      Debug.Out("Result res : " & DebugFmt(new));
      RETURN new
    END
  END Unify;
  
BEGIN END BnfEdit.
