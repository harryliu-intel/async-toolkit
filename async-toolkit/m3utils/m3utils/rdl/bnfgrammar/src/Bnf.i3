INTERFACE Bnf;

(* this interface should be made entirely functional

   objects should be opaque and immutable

   hide implementations and always call T.init() at the end of
   every method/procedure that returns a T
*)

IMPORT BnfType;

TYPE
  T <: Public;

  Public = BnfType.T OBJECT
  METHODS
    init() : T; (* init hash value *)
    replaceChild(old, new : T);
    equal(x : T) : BOOLEAN;
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

TYPE Editor = PROCEDURE ( t : T;
                          seq : REFANY (* TextBnfSeq.T *);
                          stringmapper : StringMapper) : T; 
     
PROCEDURE DistributeAll(t : T;
                        seq : REFANY (* TextBnfSeq.T *);
                        stringMapper : StringMapper) : T;
  (* distribute everything else over Disjunction 
     (move Disjunction to top of tree, result will have no child Disjunctions)
  *)

TYPE StringMapper = PROCEDURE ( t : TEXT ) : TEXT;
     
PROCEDURE RemoveSeqLists(t : T;
                         seq : REFANY (* TextBnfSeq.T *);
                         stringMapper : StringMapper) : T;

PROCEDURE RemoveIdentLists(t : T;
                           seq : REFANY (* TextBnfSeq.T *);
                           stringMapper : StringMapper) : T;

PROCEDURE RemoveOptionalStringIdent(t : T;
                                    seq : REFANY (* TextBnfSeq.T *);
                                    stringMapper : StringMapper) : T;

PROCEDURE RemoveNestedSequences(t : T;
                                seq : REFANY (* TextBnfSeq.T *);
                                stringMapper : StringMapper) : T;

PROCEDURE RemoveSingletonSequences(t : T;
                                   seq : REFANY (* TextBnfSeq.T *);
                                   stringMapper : StringMapper) : T;

PROCEDURE RemoveRemainingOptionals(t : T;
                                   seq : REFANY (* TextBnfSeq.T *);
                                   stringMapper : StringMapper) : T;

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE DebugBnf(a : T; lev : CARDINAL) : TEXT;

  (********************   MAKERS   ********************)
  
PROCEDURE MakeSequence(READONLY of : ARRAY OF T) : Sequence;

PROCEDURE MakeDisjunction(READONLY of : ARRAY OF T) : Disjunction;

PROCEDURE MakeOptional(of : T) : Optional;

PROCEDURE MakeListOf(of : T) : ListOf;

PROCEDURE MakeIdent(nm : TEXT) : Ident;

PROCEDURE MakeString(str : TEXT) : String;
  
END Bnf.
