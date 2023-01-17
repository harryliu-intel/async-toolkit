MODULE TraceInterpolator;
IMPORT Trace;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Debug;
IMPORT Rd;

CONST LR = LongReal;

REVEAL
  T = Public BRANDED Brand OBJECT
    tr         : Trace.T;
    time, data : REF ARRAY OF LONGREAL;
    
  OVERRIDES
    init := Init;
    minT := MinT;
    maxT := MaxT;
    eval := Eval;
  END;

PROCEDURE Init(t : T; tr : Trace.T; idx : Trace.NodeId; scratch : REF Array) : T
  RAISES { Rd.EndOfFile, Rd.Failure } =
  VAR
    n := tr.getSteps();
  BEGIN
    t.tr := tr;
    t.time := tr.sharedTime();
    IF scratch # NIL THEN
      IF scratch^ = NIL OR NUMBER(scratch^^) # n THEN
        scratch^ := NEW(REF ARRAY OF LONGREAL, n);
      END;
      t.data := scratch^
    ELSE
      t.data := NEW(REF ARRAY OF LONGREAL, n);
    END;
    tr.getNodeData(idx, t.data^);
    RETURN t
  END Init;

PROCEDURE  MinT(t : T) : LONGREAL =
  BEGIN
    RETURN t.time[0]
  END MinT;
  
PROCEDURE  MaxT(t : T) : LONGREAL =
  BEGIN
    RETURN t.time[LAST(t.time^)]
  END MaxT;
  
PROCEDURE  Eval(t : T; time : LONGREAL) : LONGREAL
  RAISES { OutOfBounds } =

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
      IF NOT ( t.time[i] <= time AND
        (i = NUMBER(t.time^) - 1 OR t.time[i + 1]> time)) THEN
        Eput("FindFloorIdx assertion failed :");
        Eput(F("seq.size=%s i=%s time=%s seq[i].at=%s",
               Int(NUMBER(t.time^)), Int(i), LR(time), LR(t.time[i]))
        );
        IF i # LAST(t.time^) THEN
          Eput(F("t.time[i + 1]=%s", LR(t.time[i + 1])))
        END;
        Debug.Error(error)
      END
    END Check;

  PROCEDURE FindLb() : CARDINAL =
    VAR
      lo := 0;
      hi := LAST(t.data^);
    BEGIN
      WHILE lo < hi DO
        WITH i  = lo + (hi - lo) DIV 2,
             ai = t.time[i]             DO
          IF time = ai THEN
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
    END FindLb;

  BEGIN
    IF time < t.time[0] OR time >  t.time[LAST(t.time^)] THEN
      RAISE OutOfBounds
    END;

    IF time = t.time[LAST(t.time^)] THEN
      RETURN t.data[LAST(t.time^)]
    ELSE
      WITH lb = FindLb(),

           lt = t.time[lb],
           ut = t.time[lb + 1],

           lx = t.data[lb],
           ux = t.data[lb + 1],

           dt  = ut   - lt,
           dt0 = time - lt,
           ft  = dt0 / dt,

           dx  = ux - lx,
           dx0 = dx * ft,

           x   = lt + dx0 DO
        RETURN x
      END
    END
  END Eval;

BEGIN END TraceInterpolator.
