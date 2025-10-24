(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TransitionFinder;
IMPORT TransitionSeq;
IMPORT Transition;
IMPORT CardTransitionSeqTbl;
IMPORT Trace;
IMPORT Rd;
IMPORT Debug;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT V01X;

CONST LR = LongReal;
      
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

PROCEDURE ForNode(t : T; id : CARDINAL; doSlew : BOOLEAN) : TransitionSeq.T
  RAISES { Rd.EndOfFile, Rd.Failure } =
  VAR
    res : TransitionSeq.T;
  BEGIN
    IF NOT t.tbl.get(id, res) THEN
      t.trace.getTimeData(t.ta^);
      t.trace.getNodeData(id, t.na^);
      res := Find(t.ta^, t.na^, t.thres, t.hysteresis, doSlew);

      IF NUMBER(t.ta^) = 0 THEN
        res.initVal := V01X.T.VX
      ELSIF t.na[0] < t.thres - t.hysteresis THEN
        res.initVal := V01X.T.V0
      ELSIF t.na[0] > t.thres + t.hysteresis THEN
        res.initVal := V01X.T.V1
      ELSE
        res.initVal := V01X.T.VX
      END;
        
      EVAL t.tbl.put(id, res)
    END;
    RETURN res
  END ForNode;

PROCEDURE Find(READONLY timea, nodea : ARRAY OF LONGREAL;
               thres, hysteresis     : LONGREAL;
               doSlew                : BOOLEAN) : TransitionSeq.T =

  PROCEDURE CheckTransitions() =
    BEGIN
      FOR i := 1 TO res.size() - 1 DO
        WITH this = res.get(i),
             prev = res.get(i - 1) DO
          <*ASSERT this.at # prev.at*>
          <*ASSERT this.dir = -prev.dir*>
        END
      END
    END CheckTransitions;

  PROCEDURE SearchBackwardForCrossing(i     : CARDINAL;
                                      trig  : LONGREAL;
                                      dirF  : LONGREAL) : LONGREAL =
    (* return the (interpolated) time at which the node crossed trig in
       the indicated direction, starting from i and working backwards in
       time *)
    BEGIN
      <* ASSERT dirF = -1.0d0 OR dirF = +1.0d0 *>
      FOR j := i TO 1 BY -1 DO
        WITH n = nodea[j], pn = nodea[j-1],
             t = timea[j], pt = timea[j-1] DO
          IF dirF * nodea[   j   ] >  dirF * trig AND
             dirF * nodea[ j - 1 ] <= dirF * trig      THEN
            WITH delV  = n - pn,
                 delT  = t - pt,
                 delV0 = trig - pn,
                 delT0 = delV0 / delV * delT,
                 at    = pt + delT0 DO
              state := 1 - state;
              RETURN at;
            END
          END
        END
      END;
      RETURN FIRST(LONGREAL) (* not found *)
    END SearchBackwardForCrossing;
    
  VAR
    res := NEW(TransitionSeq.T).init();
    state : [0..1];
    dir : [ -1 .. +1 ];
    dirF : LONGREAL;
    slew : LONGREAL;
    trans : Transition.T;
  BEGIN
    <*ASSERT NUMBER(timea) = NUMBER(nodea)*>
    <*ASSERT hysteresis >= 0.0d0 *>
    
    (* starting state *)
    IF nodea[0] < thres THEN state := 0 ELSE state := 1 END;
    
    FOR i := FIRST(timea) TO LAST(timea) DO
      <*ASSERT i = 0 OR timea[i] > timea[i - 1]*>
      CASE state OF
        0 => dir := +1
      |
        1 => dir := -1
      END;

      dirF := FLOAT(dir, LONGREAL);
      
      IF dirF * nodea[i] > dirF * (thres + dirF * hysteresis) THEN
        (* search backward for the actual crossing point, it must exist *)
        WITH crossTime = SearchBackwardForCrossing(i, thres, dirF) DO
          <*ASSERT crossTime # FIRST(LONGREAL) *>
          trans := Transition.T { at := crossTime, dir := dir };
 
          IF doSlew THEN
            WITH upp = SearchBackwardForCrossing(i, thres + dirF * hysteresis, dirF),
                 low = SearchBackwardForCrossing(i, thres - dirF * hysteresis, dirF) DO
              (* upper must exist since that was the original guard of this IF
                 block 
                 
                 I am guessing lower must exist also.
              *)

              <*ASSERT upp # FIRST(LONGREAL)*>
              IF low = FIRST(LONGREAL) THEN
                (* do the half slew instead *)
                slew := hysteresis / (upp - crossTime)
              ELSE
                (* do the full slew *)
                slew := (2.0d0 * hysteresis) / (upp - low)
              END;

              <*ASSERT slew >= 0.0d0*>
              trans.slew := slew
            END
          END;

          res.addhi(trans)
        END
      END
    END;
      
    CheckTransitions();
    
    RETURN res
  END Find;

PROCEDURE FindValueAt(seq : TransitionSeq.T; time : LONGREAL) : V01X.T =
  BEGIN
    WITH flIdx = FindFloorIdx(seq, time) DO
      IF flIdx = -1 THEN
        RETURN seq.initVal
      ELSE
        WITH tr = seq.get(flIdx) DO
          CASE tr.dir OF
            -1 => RETURN V01X.T.V0
          |
            +1 => RETURN V01X.T.V1
          |
             0 => Debug.Warning("Transition dir = 0"); RETURN V01X.T.VX
          END
        END
      END
    END
  END FindValueAt;
  
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

  VAR error := "";

  PROCEDURE Eput(msg : TEXT) =
    BEGIN
      error := error & msg & "\n"
    END Eput;
    
  PROCEDURE Check(i : [-1.. LAST(CARDINAL)]) =
    BEGIN
      IF NOT ( seq.get(i).at <= time AND
        (i = seq.size() - 1 OR seq.get(i + 1).at > time)) THEN
        Eput("FindFloorIdx assertion failed :");
        Eput(F("seq.size=%s i=%s time=%s seq[i].at=%s",
               Int(seq.size()), Int(i), LR(time), LR(seq.get(i).at))
        );
        IF i # seq.size() - 1 THEN
          Eput(F("seq[i+1].at=%s", LR(seq.get(i+1).at)))
        END;
        Debug.Error(error)
      END
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
