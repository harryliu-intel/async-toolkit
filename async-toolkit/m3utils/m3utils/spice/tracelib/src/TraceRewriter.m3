MODULE TraceRewriter;
IMPORT Pathname;
IMPORT Trace;
IMPORT TraceOp;
IMPORT TextSeq;

REVEAL
  T = Public BRANDED Brand OBJECT
    root         : Pathname.T;
    rewriterPath : Pathname.T;
    tr           : Trace.T;
    dirty        : BOOLEAN;
    scratch      : REF ARRAY OF LONGREAL;
  OVERRIDES
    init    := Init;
    sync    := Sync;
    addhiOp := AddhiOp;
  END;

PROCEDURE Init(t : T; root : Pathname.T; rewriterPath : Pathname.T) : T =
  BEGIN
    t.root         := root;
    t.rewriterPath := rewriterPath;
    RETURN t
  END Init;

PROCEDURE Sync(t : T) =
  BEGIN
  END Sync;

PROCEDURE AddhiOp(t            : T;
                  op           : TraceOp.T;
                  aliases      : TextSeq.T;
                  relPrec      : LONGREAL;
                  noArith      : BOOLEAN) =
  BEGIN
    IF t.scratch = NIL OR NUMBER(t.scratch^) # t.tr.getSteps() THEN
      t.scratch := NEW(REF ARRAY OF LONGREAL, t.tr.getSteps())
    END;
    op.exec(t.tr, t.scratch^)
  END AddhiOp;

BEGIN END TraceRewriter.
