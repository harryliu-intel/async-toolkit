MODULE ConstrainedSpace;
IMPORT LRMatrix2 AS Mat;
FROM LRMatrix2 IMPORT MulSV, V, M, SumSqV, MulMV, LinearCombinationV, SubV,
                      FormatM, FormatV;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal, Int;
FROM Math IMPORT sqrt, sin, cos;

REVEAL
  T = Public BRANDED OBJECT
    n         : CARDINAL;
    radius    : LONGREAL;
    principal : REF V;
    basis     : REF M; (* principal should be column 0 of this *)
  OVERRIDES
    init := InitCS;

    cons2cart := Cons2Cart;
    (* map a N-1 p in constrained space to N in cartesian space *) 
  END;

PROCEDURE DotV(READONLY a, b : V) : LONGREAL =
  VAR
    z := 0.0d0;
  BEGIN
    <*ASSERT NUMBER(a) = NUMBER(b)*>
    FOR i := FIRST(a) TO LAST(a) DO
      z := z + a[i]*b[i]
    END;
    RETURN z
  END DotV;

PROCEDURE NormalizeV(READONLY from : V; VAR to : V) =
  BEGIN
    WITH norm = sqrt(SumSqV(from)) DO
      MulSV(1.0d0/norm, from, to)
    END
  END NormalizeV;

PROCEDURE TransposeM(VAR m : M) =
  VAR
    temp : LONGREAL;
    n := NUMBER(m);
  BEGIN
    <*ASSERT NUMBER(m) = NUMBER(m[0])*>
    FOR i := 1 TO n-1 DO
      FOR j := 0 TO i-1 DO
        (* go over the lower left *)
        temp := m[i,j];
        m[i,j] := m[j,i];
        m[j,i] := temp
      END
    END
  END TransposeM;

PROCEDURE GetMaxIdx(READONLY v : V) : CARDINAL =
  VAR
    idx := FIRST(v);
  BEGIN
    FOR i := FIRST(v)+1 TO LAST(v) DO
      IF ABS(v[i]) > ABS(v[idx]) THEN idx := i END
    END;
    RETURN idx
  END GetMaxIdx;
  
CONST Verbose = TRUE;
      
PROCEDURE InitCS(cs         : T;
                 r          : LONGREAL;
                 READONLY p : V) : T =
  VAR
    n := NUMBER(p);
    temp := NEW(REF V, n);
  BEGIN
    cs.n := n;
    cs.basis := NEW(REF M, n, n);

    IF Verbose THEN
      Debug.Out(F("ConstrainedSpace.InitCS r=%s p=\n%s",
                  LongReal(r), FormatV(p)))
    END;
    
    Mat.MakeUnit(cs.basis^); (* make it the unit matrix, we will use it for ROW
                             vectors and transpose at end *)
    
    cs.radius := r;
    cs.principal := NEW(REF V, n);

    IF SumSqV(p) = 0.0d0 THEN
      (* handle the zero case *)
      cs.principal^ := p;
      cs.principal[0] := 1.0d0
    ELSE
      NormalizeV(p, cs.principal^)
    END;

    IF Verbose THEN
      Debug.Out(F("ConstrainedSpace.InitCS cs.principal=\n%s",
                  FormatV(cs.principal^)))
    END;
    
    (* find largest element of p and swap it with that entry in the cs.basis *)
    WITH idx = GetMaxIdx(cs.principal^) DO
      Debug.Out("ConstrainedSpace.InitCS idx=" & Int(idx));
      cs.basis[idx] := cs.basis[0]
    END;

    (* Gram-Schmidt orthogonalization starting from cs.principal *)
    cs.basis[0] := cs.principal^;

    FOR k := 1 TO n-1 DO
      FOR j := 0 TO k-1 DO
        WITH dp = DotV(cs.basis[k],cs.basis[j]) DO
          MulSV(dp, cs.basis[j], temp^);
          SubV(cs.basis[k], temp^, cs.basis[k]);
        END;
        Debug.Out(F("ConstrainedSpace.InitCS basis[%s]=\n%s",
                    Int(k), FormatV(cs.basis[k])));
        <*ASSERT SumSqV(cs.basis[k]) # 0.0d0*>
        NormalizeV(cs.basis[k], cs.basis[k]);
      END
    END;

    (* cs.basis should now be an orthonormal set *)
    TransposeM(cs.basis^);

    Debug.Out(F("cs.basis=\n%s", Mat.FormatM(cs.basis^)));
    RETURN cs
  END InitCS;

PROCEDURE Cons2Cart(cs : T; READONLY x : V; VAR y : V) =
  VAR
    temp := NEW(REF V, cs.n);
    s    := sqrt(SumSqV(x));
    phi  := s/cs.radius;
  BEGIN
    <*ASSERT NUMBER(x) = cs.n-1*>
    <*ASSERT NUMBER(y) = cs.n*>
    IF s = 0.0d0 THEN
      FOR i := FIRST(y) TO LAST(y) DO y[i] := 0.0d0 END;
      RETURN
    END;
    temp[0] := 0.0d0;
    SUBARRAY(temp^,1,cs.n-1) := x;
    IF Verbose THEN Debug.Out("starting temp="&FormatV(temp^)) END;
    MulMV(cs.basis^, temp^, temp^);
    NormalizeV(temp^, temp^);
    IF Verbose THEN
      Debug.Out("transformed temp="&FormatV(temp^));
      Debug.Out("norm of transformed temp=" & LongReal(sqrt(SumSqV(temp^))));
    END;
    (* 
       temp is now a normalized vector pointing in N-space

       we consider it tangent to the constraint surface at point p.

       angular arc is phi
    *)
    LinearCombinationV(cs.radius*cos(phi), cs.principal^,
                       cs.radius*sin(phi),         temp^,
                       y);
    IF Verbose THEN
      Debug.Out(F("Cons2Cart : cs.basis=\n%sx=%s\ns=%s\nphi=%s\ny=%s\n",
                  FormatM(cs.basis^), FormatV(x), LongReal(s), LongReal(phi),
                  FormatV(y)) &
                  F("cs.principal=%s\ntemp=%s",
                    FormatV(cs.principal^), FormatV(temp^)))
    END
  END Cons2Cart;
  
BEGIN END ConstrainedSpace.
