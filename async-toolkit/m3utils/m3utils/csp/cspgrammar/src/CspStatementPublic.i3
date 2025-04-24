INTERFACE CspStatementPublic;
IMPORT CspStatement;
IMPORT CspGuardedCommandSeq;
IMPORT CspStatementSeq;

REVEAL
  CspStatement.Guarded = CspStatement.T BRANDED CspStatement.Brand & " Guarded" OBJECT
    gcs : CspGuardedCommandSeq.T;
  END;

  CspStatement.Compound = CspStatement.T BRANDED CspStatement.Brand & " Compound" OBJECT
    stmts : CspStatementSeq.T;
  END;

END CspStatementPublic.
 
