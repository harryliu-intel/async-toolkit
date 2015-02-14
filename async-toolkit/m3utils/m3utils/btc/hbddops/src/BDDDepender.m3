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
    init      := Init;
    depends   := Depends;
    isPlainOr := IsPlainOr;
    flush     := Flush;
  END;

PROCEDURE Flush(t : T; b : BDD.T) =
  VAR s : BDDSet.T; BEGIN EVAL t.cache.delete(b, s) END Flush;

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

PROCEDURE AllocIfNull(VAR dep : T) =
  BEGIN
    IF dep = NIL THEN
      dep  := NEW(T).init();
    END
  END AllocIfNull;

BEGIN END BDDDepender.
