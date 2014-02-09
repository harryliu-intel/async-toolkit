(* $Id$ *)

MODULE BDDDepender;
IMPORT BDDBDDSetTbl;
IMPORT BDD, BDDSet;
IMPORT BDDDepends;
IMPORT BDDOpsH;

REVEAL
  T = Public BRANDED Brand OBJECT
    cache : BDDBDDSetTbl.T; 
  OVERRIDES
    init := Init;
    depends := Depends;
    isPlainOr := IsPlainOr;
  END;

PROCEDURE IsPlainOr(t : T; b : BDD.T) : BOOLEAN =
  VAR q := t.depends(b); 
      z := BDDOpsH.AccumulateBDD(q, BDD.Or, BDD.False());
  BEGIN
    RETURN b = z
  END IsPlainOr;

PROCEDURE Init(t : T) : T = 
  BEGIN t.cache := NEW(BDDBDDSetTbl.Default).init(); RETURN t END Init;

PROCEDURE Depends(t : T; b : BDD.T) : BDDSet.T =
  VAR s : BDDSet.T; BEGIN
    IF NOT t.cache.get(b, s) THEN 
      s := BDDDepends.Depends(b);
      EVAL t.cache.put(b, s)
    END;
    RETURN s
  END Depends;


BEGIN END BDDDepender.
