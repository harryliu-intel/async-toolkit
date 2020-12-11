MODULE Bnf;
TYPE
  Array = REF ARRAY OF T;
  
REVEAL
  T = Public BRANDED Brand OBJECT
  END;

  Ident = PubIdent BRANDED Brand & " Ident" OBJECT
  OVERRIDES
    copy := CopyIdent;
    deepCopy := CopyIdent;
  END;
  
  String = PubString BRANDED Brand & " String" OBJECT
  OVERRIDES
    copy := CopyString;
    deepCopy := CopyString;
  END;
  
  ListOf = PubListOf BRANDED Brand & " ListOf" OBJECT
  OVERRIDES
    copy := CopyListOf;
    deepCopy := DeepCopyListOf;
    replaceChild := ReplaceChildListOf;
  END;
  
  Optional = PubOptional BRANDED Brand & " Optional" OBJECT
  OVERRIDES
    copy := CopyOptional;
    deepCopy := DeepCopyOptional;
    replaceChild := ReplaceChildOptional;
  END;
  
  Disjunction = PubDisjunction BRANDED Brand & " Disjunction" OBJECT
  OVERRIDES
    copy := CopyDisjunction;
    deepCopy := DeepCopyDisjunction;
    replaceChild := ReplaceChildDisjunction;
  END;
  
  Sequence = PubSequence BRANDED Brand & " Sequence" OBJECT
  OVERRIDES
    copy := CopySequence;
    deepCopy := DeepCopySequence;
    replaceChild := ReplaceChildSequence;
  END;

PROCEDURE CopyIdent(t : T) : T = BEGIN RETURN t END CopyIdent;

CONST CopyString = CopyIdent;

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

PROCEDURE DistributeAll(t : T) : T =
  VAR elems := NEW(Array, 1);
      root := NEW(Disjunction, elems := elems);
  BEGIN
    elems[0] := t;
    DistributeRec(root, t);
    <*ASSERT NUMBER(root.elems^) = 1*>
    RETURN root.elems[0]
  END DistributeAll;
  
BEGIN END Bnf.
