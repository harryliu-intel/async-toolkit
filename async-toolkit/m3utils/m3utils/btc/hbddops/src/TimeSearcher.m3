(* $Id: TimeSearcher.m3,v 1.2 2014/03/22 20:47:37 mika Exp $ *)

MODULE TimeSearcher;
IMPORT BDDDepender;
IMPORT BDD, BDDBDDTbl;
IMPORT BDDCausal;
FROM BDDOpsH IMPORT XFormat;

REVEAL
  T = PublicTS BRANDED Brand OBJECT
  OVERRIDES
    init         := Init;
    debugFmt     := DebugFmt;
  END;

PROCEDURE Init(t : T; eqns : BDDBDDTbl.T) : T =
  CONST
    DefaultDelay      = 1.0d0;
    DefaultSourceTime = 0.0d0;
  VAR
    b, q, a : BDD.T;
  BEGIN
    EVAL BDDCausal.T.init(t);

    (* initialize *)

    WITH     depender = NEW(BDDDepender.T).init(),
             iter     = eqns.iterate() DO
      WHILE iter.next(b, q) DO
        WITH dep = depender.depends(q) DO
          IF q = NIL THEN
            (* a source *)
            t.setSource(b, DefaultSourceTime)
          ELSE
            WITH jter = dep.iterate() DO
              WHILE jter.next(a) DO
                t.addDependency(a, b, sync := FALSE);
                t.setDelay(a, b, DefaultDelay)
              END
            END
          END
        END
      END
    END;

    RETURN t
  END Init;

PROCEDURE DebugFmt(<*UNUSED*>t : T; b : BDD.T) : TEXT =
  BEGIN RETURN XFormat(b) END DebugFmt;

BEGIN END TimeSearcher.
