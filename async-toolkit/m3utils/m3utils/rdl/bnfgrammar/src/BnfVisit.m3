MODULE BnfVisit;
IMPORT Bnf;
FROM Bnf IMPORT ListOf, Optional, Sequence, Disjunction;

PROCEDURE Pre(t : Bnf.T; visitor : T) =
  BEGIN
    visitor.visit(t);
    VisitChildren(t, visitor, Pre)
  END Pre;

PROCEDURE Post(t : Bnf.T; visitor : T) =
  BEGIN
    VisitChildren(t, visitor, Post);
    visitor.visit(t);
  END Post;

TYPE Recurse = PROCEDURE (t : Bnf.T; visitor : T);
     
PROCEDURE VisitChildren(t : Bnf.T; visitor : T; recurse : Recurse) =
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

BEGIN END BnfVisit.
