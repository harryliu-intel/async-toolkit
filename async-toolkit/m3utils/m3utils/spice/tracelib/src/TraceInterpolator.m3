MODULE TraceInterpolator;
IMPORT Trace;
FROM Fmt IMPORT F, Int, LongReal, FN;
IMPORT Debug;
IMPORT Rd;

VAR doDebug := Debug.DebugThis("TraceInterpolator");
    
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
    integrate := Integrate;
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

PROCEDURE FindLb(READONLY time : ARRAY OF LONGREAL; t : LONGREAL) : CARDINAL =

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

  VAR
    lo := 0;
    hi := LAST(time);
  BEGIN
    WHILE lo < hi DO
      WITH i  = lo + (hi - lo) DIV 2,
           ai = time[i]             DO
        IF    t = ai THEN
          RETURN i
        ELSIF t > ai THEN
          lo := i + 1
        ELSE
          hi := i
        END
      END
    END;
    <*ASSERT lo = hi*>
    Check(lo - 1, time, t);
    RETURN lo - 1
  END FindLb;
  
PROCEDURE Check(i : [-1.. LAST(CARDINAL)];
                READONLY time : ARRAY OF LONGREAL;
                t : LONGREAL) =
  VAR error := "";

  PROCEDURE Eput(msg : TEXT) =
    BEGIN
      error := error & msg & "\n"
    END Eput;

  BEGIN
    IF NOT ( time[i] <= t AND
      (i = NUMBER(time) - 1 OR time[i + 1] > t)) THEN
      Eput("FindFloorIdx assertion failed :");
      Eput(F("seq.size=%s i=%s time=%s seq[i].at=%s",
             Int(NUMBER(time)), Int(i), LR(t), LR(time[i]))
      );
      IF i # LAST(time) THEN
        Eput(F("time[i + 1]=%s", LR(time[i + 1])))
      END;
      Debug.Error(error)
    END
  END Check;

PROCEDURE BoundsCheck(READONLY time : ARRAY OF LONGREAL; t : LONGREAL)
  RAISES { OutOfBounds } =
  BEGIN
    IF t < time[0] OR t > time[LAST(time)] THEN
      RAISE OutOfBounds
    END;
  END BoundsCheck;
  
PROCEDURE  Eval(t : T; time : LONGREAL) : LONGREAL
  RAISES { OutOfBounds } =
  BEGIN
    BoundsCheck(t.time^, time);
    
    IF time = t.time[LAST(t.time^)] THEN
      RETURN t.data[LAST(t.time^)]
    ELSE
      WITH lb = FindLb(t.time^, time),

           lt = t.time[lb],
           ut = t.time[lb + 1],

           lx = t.data[lb],
           ux = t.data[lb + 1],

           dt  = ut   - lt,
           dt0 = time - lt,
           ft  = dt0 / dt,

           dx  = ux - lx,
           dx0 = dx * ft,

           x   = lx + dx0 DO
        IF doDebug THEN
          Debug.Out(FN("lb=%s lt=%s ut=%s lx=%s ux=%s dt=%s dt0=%s ft=%s dx=%s dx0=%s x=%s",
                       ARRAY OF TEXT { Int(lb),
                                       LR(lt),
                                       LR(ut),
                                       LR(lx),
                                       LR(ux),
                                       LR(dt),
                                       LR(dt0),
                                       LR(ft),
                                       LR(dx),
                                       LR(dx0),
                                       LR(x)}))
        END;
        
        RETURN x
      END
    END
  END Eval;

PROCEDURE Integrate(t : T; a, b : LONGREAL) : LONGREAL
  RAISES { OutOfBounds } =
  VAR
    ay  := Eval(t, a);
    alb := FindLb(t.time^, a);
    by  := Eval(t, b);
    blb := FindLb(t.time^, b);
  BEGIN
    IF    b < a THEN
      RETURN 0.0d0
    ELSIF alb = blb THEN
      RETURN 0.5d0 * (ay + by) * (b - a)
    ELSE
      VAR
        sum := 0.0d0;

      PROCEDURE Seg(x0, y0, x1, y1 : LONGREAL) =
        BEGIN
          sum := sum + 0.5d0 * (y0 + y1) * (x1 - x0)
        END Seg;
        
      BEGIN
        (* first interval *)
        Seg(a, ay, t.time[alb + 1], t.data[alb + 1]);

        FOR i := alb + 1 TO blb - 1 DO
          (* i runs over the low index of each interval that's not the
             last nor the first *)
          Seg(t.time[i], t.data[i], t.time[i + 1], t.data[i + 1])
        END;

        (* last interval *)
        Seg(t.time[blb], t.data[blb], b, by);

        RETURN sum
      END
    END
  END Integrate;

BEGIN END TraceInterpolator.
