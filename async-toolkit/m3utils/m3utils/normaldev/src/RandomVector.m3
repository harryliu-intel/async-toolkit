MODULE RandomVector;

(* use Muller-Marsaglia-Knuth algorithm for a random vector on the N-1-sphere
   (surface of the N-ball) 

   See 

   https://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/

   Author : mika.nystroem@intel.com
   August, 2024

*)
IMPORT NormalDeviate;
IMPORT Math;
IMPORT Random;

PROCEDURE GetDir(rand    : Random.T;     (* random generator     *)
                 r       : Base;         (* radius               *)
                 VAR v   : ARRAY OF Base (* workspace and output *)
  ) =
  BEGIN
    FOR i := FIRST(v) TO LAST(v) - 1 BY 2 DO
      NormalDeviate.Get2(rand, 0.0d0, 1.0d0, v[i], v[i + 1])
    END;
    IF NUMBER(v) MOD 2 = 1 THEN
      v[LAST(v)] := NormalDeviate.Get(rand, 0.0d0, 1.0d0)
    END;
    VAR
      sumsq := 0.0d0;
    BEGIN
      IF NUMBER(v) # 0 THEN
        FOR i := FIRST(v) TO LAST(v) DO
          sumsq := sumsq + v[i] * v[i]
        END;
        WITH d    = Math.sqrt(sumsq),
             mult = r / d DO
          FOR i := FIRST(v) TO LAST(v) DO
            v[i] := v[i] * mult
          END
        END
      END
    END
  END GetDir;
  
PROCEDURE GetDirV(rand    : Random.T;     (* random generator     *)
                  dims    : CARDINAL;
                  r       : Base          (* radius               *)
  ) : REF ARRAY OF Base =
  BEGIN
    WITH res = NEW(REF ARRAY OF Base, dims) DO
      GetDir(rand, r, res^);
      RETURN res
    END
  END GetDirV;


PROCEDURE GetPoint(rand    : Random.T;     (* random generator     *)
                   r       : Base;         (* radius               *)
                   VAR v   : ARRAY OF Base (* workspace and output *)
  ) =
  VAR
    dims := FLOAT(MAX(NUMBER(v), 1), Base);
    (* avoid div by zero for trivial 0D *)
    
    rr := Math.pow(rand.longreal(0.0d0, 1.0d0), 1.0d0 / dims);
    mult := rr * r;
  BEGIN
    GetDir(rand, r, v);
    FOR i := FIRST(v) TO LAST(v) DO
      v[i] := v[i] * mult
    END
  END GetPoint;

  
PROCEDURE GetPointV(rand    : Random.T;     (* random generator     *)
                    dims    : CARDINAL;
                    r       : Base          (* radius               *)
  ) : REF ARRAY OF Base =
  BEGIN
    WITH res = NEW(REF ARRAY OF Base, dims) DO
      GetPoint(rand, r, res^);
      RETURN res
    END
  END GetPointV;

BEGIN END RandomVector.
