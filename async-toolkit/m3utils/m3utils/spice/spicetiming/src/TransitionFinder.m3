MODULE TransitionFinder;
IMPORT TransitionSeq;
IMPORT Transition;
IMPORT CardTransitionSeqTbl;
IMPORT Trace;
IMPORT Rd;

REVEAL
  T = Public BRANDED Brand OBJECT
    trace  : Trace.T;
    tbl    : CardTransitionSeqTbl.T;
    na, ta : REF ARRAY OF LONGREAL;
    thres, hysteresis : LONGREAL;
  OVERRIDES
    init := Init;
    forNode := ForNode;
  END;

PROCEDURE Init(t : T; trace : Trace.T; thres, hysteresis : LONGREAL) : T =
  BEGIN
    t.trace := trace;
    t.ta := NEW(REF ARRAY OF LONGREAL, t.trace.getSteps());
    t.na := NEW(REF ARRAY OF LONGREAL, t.trace.getSteps());
    t.thres := thres;
    t.hysteresis := hysteresis;
    t.tbl := NEW(CardTransitionSeqTbl.Default).init();
    RETURN t
  END Init;

PROCEDURE ForNode(t : T; id : CARDINAL) : TransitionSeq.T
  RAISES { Rd.EndOfFile, Rd.Failure } =
  VAR
    res : TransitionSeq.T;
  BEGIN
    IF NOT t.tbl.get(id, res) THEN
      t.trace.getTimeData(t.ta^);
      t.trace.getNodeData(id, t.na^);
      res := Find(t.ta^, t.na^, t.thres, t.hysteresis);
      EVAL t.tbl.put(id, res)
    END;
    RETURN res
  END ForNode;

PROCEDURE Find(READONLY timea, nodea : ARRAY OF LONGREAL;
               thres, hysteresis     : LONGREAL) : TransitionSeq.T =
  VAR
    res := NEW(TransitionSeq.T).init();
    state : [0..1];
  BEGIN
    <*ASSERT NUMBER(timea) = NUMBER(nodea)*>
    <*ASSERT hysteresis >= 0.0d0 *>
    
    (* starting state *)
    IF nodea[0] < thres THEN state := 0 ELSE state := 1 END;
    
    FOR i := FIRST(timea) TO LAST(timea) DO
      CASE state OF
        0 =>
        IF nodea[i] > thres + hysteresis THEN
          (* search backward for the actual crossing point, it must exist *)
          FOR j := i TO 1 BY -1 DO
            WITH n = nodea[j], pn = nodea[j-1],
                 t = timea[j], pt = timea[j-1] DO
              IF nodea[j] > thres AND nodea[j-1] <= thres THEN
                WITH delV  = n - pn,
                     delT  = t - pt,
                     delV0 = thres - pn,
                     delT0 = delV0 / delV * delT,
                     at    = pt + delT0 DO
                  state := 1 - state;
                  res.addhi(Transition.T { at, +1 } );
                  EXIT
                END
              END
            END
          END
        END
      |
        1 =>
        IF nodea[i] < thres - hysteresis THEN
          (* search backward for the actual crossing point, it must exist *)
          FOR j := i TO 1 BY -1 DO
            WITH n = nodea[j], pn = nodea[j-1],
                 t = timea[j], pt = timea[j-1]  DO
              IF nodea[j] < thres AND nodea[j-1] >= thres THEN
                WITH delV  = n - pn,
                     delT  = t - pt,
                     delV0 = thres - pn,
                     delT0 = delV0 / delV * delT,
                     at    = pt + delT0 DO
                  state := 1 - state;
                  res.addhi(Transition.T { at, -1 } );
                  EXIT
                END
              END
            END
          END
        END
      END
    END;
    RETURN res
  END Find;
  
PROCEDURE FindFloorIdx(seq  : TransitionSeq.T;
                       time : LONGREAL) : [-1..LAST(CARDINAL) ] =

  (*
  def binarySearch(A, X):
    low, high = 0, len(A)
    while low < high:
        i = low + (high - low) // 2
        if X == A[i]:
            return i
        elif X > A[i]:
            low = i + 1
        else: # X < A[i]
            high = i
    return -1
  *)

  (* invariant 
     i \in [ lo, hi )
  *)

  PROCEDURE Check(i : [-1.. LAST(CARDINAL)]) =
    BEGIN
      <*ASSERT seq.get(i).at <= time AND
               (i = seq.size() - 1 OR seq.get(i + 1).at > time) *>
    END Check;
    
  VAR
    lo := 0;
    hi := seq.size(); (* limit of considered range (out of bounds) *)
  BEGIN
    <*ASSERT time >= 0.0d0*>
    IF seq.size() = 0 OR time < seq.get(0).at THEN RETURN -1 END;
    
    WHILE lo < hi DO
      WITH i  = lo + (hi - lo) DIV 2,
           ai = seq.get(i).at             DO
        IF time = ai THEN
          Check(i);
          RETURN i
        ELSIF time > ai THEN
          lo := i + 1
        ELSE
          hi := i
        END
      END
    END;
    <*ASSERT lo = hi*>
    Check(lo - 1);
    RETURN lo - 1
  END FindFloorIdx;

PROCEDURE FilterDir(seq : TransitionSeq.T;
                    dir : Transition.Dir) : TransitionSeq.T =
  VAR
    res := NEW(TransitionSeq.T).init();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH tran = seq.get(i) DO
        IF tran.dir = dir THEN
          res.addhi(tran)
        END
      END
    END;
    RETURN res
  END FilterDir;

PROCEDURE FilterTime(seq : TransitionSeq.T;
                     lo, hi : LONGREAL) : TransitionSeq.T =
  VAR
    res := NEW(TransitionSeq.T).init();
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH tran = seq.get(i) DO
        IF tran.at >= lo AND tran.at < hi THEN
          res.addhi(tran)
        END
      END
    END;
    RETURN res
  END FilterTime;


BEGIN END TransitionFinder.
