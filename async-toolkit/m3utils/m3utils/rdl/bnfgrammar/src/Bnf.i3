INTERFACE Bnf;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    copy() : T;
    deepCopy() : T;
    replaceChild(old, new : T);
  END;

  Visitor = OBJECT METHODS visit(t : T)  END;

PROCEDURE VisitPre(t : T; visitor : Visitor);

PROCEDURE VisitPost(t : T; visitor : Visitor);


  (********** subtypes of T **********)
TYPE  
  Ident       <: PubIdent;
  PubIdent = T OBJECT ident  : TEXT; def : T := NIL END;
  (* generally, on the parsing pass, we fill in ident

     meanwhile we build the symbol table based on the LHS of the syntax rule

     we then do DFS and back-patch the def from the symbol table *)

  String      <: PubString;
  PubString = T OBJECT string : TEXT END;

  ListOf      <: PubListOf;
  PubListOf = T OBJECT elem   : T END;

  Optional    <: PubOptional;
  PubOptional = T OBJECT elem   : T END;

  Disjunction <: PubDisjunction;
  PubDisjunction = T OBJECT elems  : REF ARRAY OF T END;

  Sequence    <: PubSequence;
  PubSequence = T OBJECT elems  : REF ARRAY OF T END;

CONST Brand = "Bnf";

PROCEDURE DistributeAll(t : T) : T;
  (* distribute everything else over Disjunction 
     (move Disjunction to top of tree, result will have no child Disjunctions)
  *)
  
END Bnf.
