(* $Id$ *)

MODULE ParametricSpline;
IMPORT Spline, CubicSpline;
IMPORT Math;

REVEAL
  T = Public BRANDED Brand OBJECT
    xSpline, ySpline : Spline.T;
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self : T; READONLY coords : ARRAY OF Coord) : T =

  PROCEDURE ComputeWeight(READONLY a, b : Coord) : LONGREAL =
    CONST MinW = 1.0d-8; (* a bit of a hack... *)
    VAR
      w : LONGREAL;
    BEGIN
      (* compute the square root of the chord length *)
      w := Math.pow((a.x-b.x)*(a.x-b.x) + (a.y-b.y)*(a.y-b.y),0.25d0);

      (* the following guarantees strictly increasing parameter values *)
      IF w = 0.0d0 THEN w := MinW END;
      
      RETURN w

    END ComputeWeight;

  VAR
    wTot := 0.0d0;
    px, py := NEW(REF ARRAY OF Coord,NUMBER(coords));
  BEGIN
    <* ASSERT NUMBER(coords) > 1 *> (* is this enough? *)
    FOR i := FIRST(coords) TO LAST(coords) - 1 DO
      wTot := wTot + ComputeWeight(coords[i],coords[i+1])
    END;

    VAR
      cur := 0.0d0;
    BEGIN
      px[0].x := cur; px[0].y := coords[0].x;
      py[0].x := cur; py[0].y := coords[0].y;

      FOR i := FIRST(coords) + 1 TO LAST(coords) DO
        cur := cur + ComputeWeight(coords[i-1],coords[i]);

        px[i].x := cur; px[i].y := coords[i].x;
        py[i].x := cur; py[i].y := coords[i].y;
      END
    END;

    (* build underlying spline functions *)

    self.xSpline := NEW(CubicSpline.T).init(px^);
    self.ySpline := NEW(CubicSpline.T).init(py^);
    
    RETURN self
  END Init;

BEGIN END ParametricSpline.
