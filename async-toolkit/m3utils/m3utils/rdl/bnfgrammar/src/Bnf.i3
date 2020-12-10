INTERFACE Bnf;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
  END;

  Visitor = OBJECT METHODS visit(t : T)  END;

PROCEDURE VisitPre(t : T; visitor : Visitor);

PROCEDURE VisitPost(t : T; visitor : Visitor);


  (********** subtypes of T **********)
TYPE  
  Ident       = T OBJECT ident  : TEXT; def : T := NIL END;
  (* generally, on the parsing pass, we fill in ident

     meanwhile we build the symbol table based on the LHS of the syntax rule

     we then do DFS and back-patch the def from the symbol table *)

  String      = T OBJECT string : TEXT END;

  ListOf      = T OBJECT elem   : T END;

  Optional    = T OBJECT elem   : T END;

  Disjunction = T OBJECT elems  : REF ARRAY OF T END;

  Sequence    = T OBJECT elems  : REF ARRAY OF T END;

CONST Brand = "Bnf";

END Bnf.
