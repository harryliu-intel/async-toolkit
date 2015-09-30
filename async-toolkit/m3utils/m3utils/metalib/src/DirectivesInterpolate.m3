MODULE DirectivesInterpolate EXPORTS Directives;
IMPORT Spline, LRPoint, CubicSpline;

<*UNUSED*>
PROCEDURE InterpolateOld(READONLY data : Characterization; 
                         at            : LONGREAL) : ARRAY Data OF LONGREAL =
  VAR p2  : CARDINAL;
      res : ARRAY Data OF LONGREAL;
  BEGIN
    IF    at < data[0,Data.SlewPoints] THEN
      p2 := 1
    ELSIF at >= data[LAST(data),Data.SlewPoints] THEN
      p2 := LAST(data)
    ELSE
      (* at >= data[0,..] AND at < data[LAST(data),...] *)
      FOR i := 0 TO NUMBER(data)-2 DO
        IF at >= data[i,Data.SlewPoints] AND 
           at < data[i+1,Data.SlewPoints]     THEN
          p2 := i + 1
        END
      END
    END;

    WITH p1   = p2 - 1,

         x1   = data[p1,Data.SlewPoints],
         x2   = data[p2,Data.SlewPoints], 

         m2   = (at-x1)/(x2-x1),
         m1   = 1.0d0 - m2                 DO
      FOR d := FIRST(Data) TO LAST(Data) DO
        res[d] := m1 * data[p1,d] + m2 * data[p2,d]
      END
    END;
    RETURN res
  END InterpolateOld;

REVEAL 
  Interpolator = BRANDED OBJECT
    spline : ARRAY Data OF Spline.T;
  END;

PROCEDURE MinMult(by : LONGREAL; whch : Data; val : LONGREAL) : LONGREAL =
  BEGIN
    IF whch IN SET OF Data { Data.MinSlew, Data.MinDelay } THEN
      RETURN val * by 
    ELSE
      RETURN val
    END
  END MinMult;

PROCEDURE MakeInterpolator(READONLY data : Characterization;
                           minMult : LONGREAL) : Interpolator =
  VAR res := NEW(Interpolator);
  BEGIN
    (* build splines *)
    FOR d := FIRST(Data) TO LAST(Data) DO
      WITH temp = NEW(REF ARRAY OF Spline.Coord, NUMBER(data)) DO
        FOR i := FIRST(data) TO LAST(data) DO
          temp[i] := LRPoint.T { data[i,Data.SlewPoints],
                                 MinMult(minMult,d,data[i,d])                };
        END;

        res.spline[d] := NEW(CubicSpline.T).init(temp^)
      END;
    END;
    RETURN res
  END MakeInterpolator;

PROCEDURE Interpolate(interpolator : Interpolator;
                      at            : LONGREAL) : ARRAY Data OF LONGREAL =
  VAR
    res : ARRAY Data OF LONGREAL;
  BEGIN
    WITH build = interpolator DO
      FOR i := FIRST(Data) TO LAST(Data) DO
        res[i] := build.spline[i].eval(at)
      END
    END;
    RETURN res
  END Interpolate;

PROCEDURE InterpolateDeriv(interpolator : Interpolator;
                      at            : LONGREAL) : ARRAY Data OF LONGREAL =
  VAR
    res : ARRAY Data OF LONGREAL;
  BEGIN
    WITH build = interpolator DO
      FOR i := FIRST(Data) TO LAST(Data) DO
        res[i] := build.spline[i].deriv(at)
      END
    END;
    RETURN res
  END InterpolateDeriv;


BEGIN END DirectivesInterpolate.
