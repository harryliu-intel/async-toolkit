MODULE Bnf;

REVEAL
  T = Public BRANDED Brand OBJECT
  END;

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
    |
      Ident(id) => (* skip *)
    ELSE
      (* skip *)
    END
  END VisitChildren;

BEGIN END Bnf.
