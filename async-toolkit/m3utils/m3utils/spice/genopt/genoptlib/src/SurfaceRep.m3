MODULE SurfaceRep;
IMPORT LRVector;
IMPORT LRMatrix2 AS M;
IMPORT Wx;
IMPORT Debug;
FROM Fmt IMPORT F, FN, Int, LongReal;

CONST LR = LongReal;
TYPE TA = ARRAY OF TEXT;

VAR doDebug := Debug.DebugThis("SurfaceRep");

PROCEDURE Cdofs(<*UNUSED*>n : CARDINAL) : CARDINAL =
  BEGIN
    RETURN 1
  END Cdofs;
  
PROCEDURE Ldofs(n : CARDINAL) : CARDINAL =
  BEGIN
    RETURN n + 1
  END Ldofs;
  
PROCEDURE Qdofs(n : CARDINAL) : CARDINAL =
  BEGIN
    RETURN (n * n + 3 * n + 2) DIV 2
  END Qdofs;
    
PROCEDURE GetConstantTerm(b : REF M.M) : LONGREAL =
  BEGIN
    WITH idx = LAST(b^) DO
      RETURN b[idx, 0]
    END
  END GetConstantTerm;

PROCEDURE ComputeIndepsQ(p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M) =
  VAR
    k := 0;
    q : LONGREAL;
    f0, f1 : LONGREAL;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
      FOR j := i TO NUMBER(p^) DO
        IF j = NUMBER(p^) THEN
          f1 := 1.0d0
        ELSE
          f1 := p[j]
        END;
        
        q := f0 * f1; (* this is the value we want *)
        
        x[row, k] := q;
        
        INC(k)
      END
    END
  END ComputeIndepsQ;

PROCEDURE ComputeIndepsL(p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M) =
  VAR
    k  := 0;
    f0 : LONGREAL;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
        
      x[row, k] := f0;
      
      INC(k)
    END
  END ComputeIndepsL;

PROCEDURE ComputeIndepsC(<*UNUSED*>p      : LRVector.T;
                         row    : CARDINAL;
                         VAR x  : M.M) =
  BEGIN
    x[row, 0] := 1.0d0
  END ComputeIndepsC;

PROCEDURE ComputeQ(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL =
  (* value of the quadratic *)
  VAR
    k := 0;
    q : LONGREAL;
    f0, f1 : LONGREAL;
    term : LONGREAL;
    sum := 0.0d0;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
      FOR j := i TO NUMBER(p^) DO
        IF j = NUMBER(p^) THEN
          f1 := 1.0d0
        ELSE
          f1 := p[j]
        END;
        q := f0 * f1;
        term := q * b[k, 0];
        sum := sum + term;
        IF wx # NIL THEN
          Wx.PutText(wx, FN("ComputeQ f0=%s f1=%s q=%s b[%s,0]=%s term=%s sum=%s\n",
                            TA{LR(f0), LR(f1), LR(q), Int(k), LR(b[k,0]),
                               LR(term), LR(sum)}))
        END;
        INC(k)
      END
    END;
    IF wx # NIL THEN
      Wx.PutText(wx,"===\n")
    END;
    RETURN sum
  END ComputeQ;

PROCEDURE ComputeL(p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL =
  VAR
    sum := 0.0d0;
    f0 : LONGREAL;
  BEGIN
    FOR i := 0 TO NUMBER(p^) DO
      IF i = NUMBER(p^) THEN
        f0 := 1.0d0
      ELSE
        f0 := p[i]
      END;
      WITH term = f0 * b[i, 0] DO
        sum := sum + term
      END
    END;
    RETURN sum
  END ComputeL;

PROCEDURE ComputeC(<*UNUSED*>p : LRVector.T; b : REF M.M; wx : Wx.T := NIL) : LONGREAL =
  BEGIN
    RETURN b[0, 0]
  END ComputeC;

PROCEDURE C2Q(n : CARDINAL; READONLY b : M.M) : REF M.M =
  BEGIN
    RETURN L2Q(n, C2L(n, b)^)
  END C2Q;

PROCEDURE Q2Q(n : CARDINAL; READONLY b : M.M) : REF M.M =
  VAR
    qdofs := Qdofs(n);
    res := NEW(REF M.M, qdofs, 1);
  BEGIN
    res^ := b;
    RETURN res
  END Q2Q;

PROCEDURE L2Q(n : CARDINAL; READONLY b : M.M) : REF M.M =
  VAR
    ldofs := Ldofs(n);
    qdofs := Qdofs(n);
    res := NEW(REF M.M, qdofs, 1);
    k := 0;
  BEGIN
    <*ASSERT NUMBER(b) = ldofs*>
    <*ASSERT NUMBER(b[0]) = 1*>
    FOR i := 0 TO n DO
      FOR j := i TO n DO
        IF    i = n THEN
          res[ k, 0 ] := b[ j, 0 ]
        ELSIF j = n THEN
          res[ k, 0 ] := b[ i, 0 ]
        ELSE
          res[ k, 0 ] := 0.0d0
        END;
        
        INC(k)
      END
    END;
    RETURN res
  END L2Q;

PROCEDURE C2L(n : CARDINAL; READONLY b : M.M) : REF M.M =
  VAR
    ldofs := Ldofs(n);
    res := NEW(REF M.M, ldofs, 1);
  BEGIN
    <*ASSERT NUMBER(b) = 1*>
    <*ASSERT NUMBER(b[0]) = 1*>
    FOR i := 0 TO ldofs - 2 DO
      res[ i, 0 ] := 0.0d0
    END;
    res[ ldofs - 1, 0 ] := b[0, 0];
    RETURN res
  END C2L;

PROCEDURE FmtC(<*UNUSED*>n : CARDINAL; b : REF M.M) : TEXT =
  BEGIN
    <*ASSERT NUMBER(b^) = 1*>
    <*ASSERT NUMBER(b[0]) = 1*>
    RETURN LR(b[0,0])
  END FmtC;

PROCEDURE FmtL(n : CARDINAL; b : REF M.M) : TEXT =
  VAR
    ldofs := Ldofs(n);
    term : TEXT;
    sum := "";
  BEGIN
    <*ASSERT NUMBER(b^) = ldofs*>
    <*ASSERT NUMBER(b[0]) = 1*>
    FOR i := 0 TO n DO
      IF i = n THEN
        term := LR(b[i, 0])
      ELSE
        term := LR(b[i, 0]) & F(" * p[%s]", Int(i))
      END;
      sum := sum & " + " & term
    END;
    RETURN sum
  END FmtL;
  
PROCEDURE FmtQ(n : CARDINAL; b : REF M.M) : TEXT =
  VAR
    k := 0;
    sum := "";
    f0, f1 : TEXT;
    q : TEXT;
    term : TEXT;
  BEGIN
    FOR i := 0 TO n DO
      IF i = n THEN
        f0 := "1"
      ELSE
        f0 := F("p[%s]", Int(i))
      END;
      FOR j := i TO n DO
        IF j = n THEN
          f1 := "1"
        ELSE
          f1 := F("p[%s]", Int(j))
        END;
        q := f0 & " * " & f1;
        term := LR(b[k, 0]) & " * " & q;
        sum := sum & " + " &  term;
        INC(k)
      END
    END;
    RETURN sum
  END FmtQ;

PROCEDURE SumAbsCoeffQ(n : CARDINAL; b : REF M.M) : ByOrder =
  VAR
    k := 0;
    f0, f1 : [0..1];
    res := ByOrder { 0.0d0, .. };
    
  BEGIN
    FOR i := 0 TO n DO
      IF i = n THEN
        f0 := 0
      ELSE
        f0 := 1
      END;
      FOR j := i TO n DO
        IF j = n THEN
          f1 := 0
        ELSE
          f1 := 1
        END;
        WITH order = f0 + f1 DO
          res[order] := res[order] + ABS(b[k,0])
        END;
        
        INC(k)
      END
    END;
    RETURN res
  END SumAbsCoeffQ;

PROCEDURE BiggestQuadratic(p : LRVector.T; b : REF M.M) : LONGREAL =
  VAR
    k := 0;
    mostNeg := LAST(LONGREAL);
  BEGIN
    (* we care about negative squared terms and any cross term *)
    FOR i := 0 TO NUMBER(p^) DO
      FOR j := i TO NUMBER(p^) DO
        IF    i = j AND i = NUMBER(p^) THEN
          (* constant term *)
          (* skip *)
        ELSIF i = j THEN
          (* squared term *)
          WITH pi = 2.0d0 * b[k, 0] DO
            mostNeg := MIN(mostNeg, pi)
          END
        ELSIF j = NUMBER(p^) THEN
          (* linear term *)
        ELSE
          (* cross term *)
          WITH pi = b[k, 0] DO
            mostNeg := MIN(-ABS(mostNeg), pi)
          END;
          WITH pj = b[k, 0] DO
            mostNeg := MIN(-ABS(mostNeg), pj)
          END
        END;
        INC(k)
      END
    END;
    RETURN -mostNeg
  END BiggestQuadratic;
  
PROCEDURE ComputeG(p : LRVector.T; b : REF M.M) : LRVector.T =
  (* gradient of the quadratic *)
  VAR
    res := NEW(LRVector.T, NUMBER(p^));
    k := 0;
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      res[i] := 0.0d0
    END;
    
    FOR i := 0 TO NUMBER(p^) DO
      FOR j := i TO NUMBER(p^) DO
        IF    i = j AND i = NUMBER(p^) THEN
          (* constant term *)
          (* skip *)
        ELSIF i = j THEN
          (* squared term *)
          WITH pi = 2.0d0 * p[i] * b[k, 0] DO
            res[i] := res[i] + pi
          END
        ELSIF j = NUMBER(p^) THEN
          (* linear term *)
          WITH pi = b[k, 0] DO
            res[i] := res[i] + pi
          END
        ELSE
          (* cross term *)
          WITH pi = p[j] * b[k, 0] DO
            res[i] := res[i] + pi
          END;
          WITH pj = p[i] * b[k, 0] DO
            res[j] := res[j] + pj
          END
        END;
        INC(k)
      END
    END;
    RETURN res
  END ComputeG;

BEGIN END SurfaceRep.
