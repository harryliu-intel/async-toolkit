(* $Id$ *)

MODULE TimeSearcher;
IMPORT BDDDepender;
IMPORT BDD, BDDSet, BDDBDDTbl, Debug, Fmt, BDDSetDef;
IMPORT BDDCausal;

REVEAL
  T = PublicTS BRANDED Brand OBJECT
    eqns : BDDBDDTbl.T;
    depender : BDDDepender.T;
  OVERRIDES
    isSource := TS_IsSource;
    predecessors := TS_Pred;
    delay := TS_Delay;
    init := Init;
  END;

PROCEDURE Init(t : T; eqns : BDDBDDTbl.T) : T =
  BEGIN
    EVAL BDDCausal.T.init(t);
    t.eqns := eqns;
    t.depender := NEW(BDDDepender.T).init();
    RETURN t
  END Init;

PROCEDURE TS_Pred(ts : T; b : BDD.T) : BDDSet.T =
  VAR
    s : BDDSet.T;
    q : BDD.T;
  BEGIN
    WITH found = ts.eqns.get(b, q) DO
      
      Debug.Out("TS_Pred(" & BDD.Format(b) & "), found="& Fmt.Bool(found), 50);
      IF q = NIL THEN 
        (* an ultimate intput *)
        RETURN NEW(BDDSetDef.T).init() 
      END;
      
      s := ts.depender.depends(q)
    END;
    RETURN s
  END TS_Pred;

PROCEDURE TS_IsSource(ts : T; 
                      b : BDD.T; VAR at : LONGREAL) : BOOLEAN =
  BEGIN
    WITH res = ts.predecessors(b).size() = 0 DO
      IF res THEN at := 0.0d0 END;
      RETURN res
    END
  END TS_IsSource;

PROCEDURE TS_Delay(<*UNUSED*>ts : T; 
                   <*UNUSED*>a, b : BDD.T) : LONGREAL =
  BEGIN 
    RETURN 1.0d0  (* for now *)
  END TS_Delay;

BEGIN END TimeSearcher.
