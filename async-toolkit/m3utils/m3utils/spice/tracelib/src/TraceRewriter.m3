MODULE TraceRewriter;
IMPORT Pathname;
IMPORT Trace;

REVEAL
  T = Public BRANDED Brand OBJECT
    root         : Pathname.T;
    rewriterPath : Pathname.T;
    tr           : Trace.T;
    dirty        : BOOLEAN;
  OVERRIDES
    init := Init;
    sync := Sync;
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

BEGIN END TraceRewriter.
