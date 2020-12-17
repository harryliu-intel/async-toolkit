INTERFACE BnfEdit;
IMPORT Bnf;
FROM Bnf IMPORT Disjunction;
IMPORT BnfRuleSeq;

TYPE T = PROCEDURE ( t : Bnf.T;
                          seq : BnfRuleSeq.T;
                          stringmapper : StringMapper) : Bnf.T; 
     
PROCEDURE DistributeAll(t : Bnf.T;
                        seq : BnfRuleSeq.T;
                        stringMapper : StringMapper) : Bnf.T;
  (* distribute everything else over Disjunction 
     (move Disjunction to top of tree, result will have no child Disjunctions)
  *)

TYPE StringMapper = PROCEDURE ( t : TEXT ) : TEXT;
     
PROCEDURE RemoveSeqLists(t : Bnf.T;
                         seq : BnfRuleSeq.T;
                         stringMapper : StringMapper) : Bnf.T;

PROCEDURE RemoveIdentLists(t : Bnf.T;
                           seq : BnfRuleSeq.T;
                           stringMapper : StringMapper) : Bnf.T;

PROCEDURE RemoveOptionalStringIdent(t : Bnf.T;
                                    seq : BnfRuleSeq.T;
                                    stringMapper : StringMapper) : Bnf.T;

PROCEDURE RemoveNestedSequences(t : Bnf.T;
                                seq : BnfRuleSeq.T;
                                stringMapper : StringMapper) : Bnf.T;

PROCEDURE RemoveSingletonSequences(t : Bnf.T;
                                   seq : BnfRuleSeq.T;
                                   stringMapper : StringMapper) : Bnf.T;

PROCEDURE RemoveRemainingOptionals(t : Bnf.T;
                                   seq : BnfRuleSeq.T;
                                   stringMapper : StringMapper) : Bnf.T;

  (********************            ********************)

PROCEDURE Substitute(t, from, to : Bnf.T) : Bnf.T;

PROCEDURE Unify(a, b : Disjunction) : Disjunction;
  (* unify two Disjunction rules *)
  
CONST Brand = "BnfEdit";

END BnfEdit.
