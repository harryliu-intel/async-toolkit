(* $Id$ *)

MODULE Main;
IMPORT Matrix, MatrixM3;
IMPORT Random;
IMPORT RefSeq;
IMPORT Time;
IMPORT IO, Fmt;

VAR r := NEW(Random.Default).init();
VAR s := NEW(RefSeq.T).init();

PROCEDURE RandomMatrix() : Matrix.T =
  VAR 
    res := NEW(Matrix.T, 10, 10);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      FOR j := FIRST(res[0]) TO LAST(res[0]) DO
        res[i,j] := r.longreal()
      END
    END;
    RETURN res
  END RandomMatrix;

VAR
  a, b, c := RandomMatrix();

CONST NumMats = 100;

BEGIN
  Matrix.MulD(a,b,c);

  FOR i := 0 TO NumMats-1 DO
    s.addhi(RandomMatrix())
  END;

  VAR
    m3Sum, fSum := 0.0d0;
    m3Start, m3Stop, fStart, fStop : Time.T;
  BEGIN

    m3Start := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        MatrixM3.MulD(s.get(i),s.get(j),c);

(*        m3Sum := m3Sum + Matrix.Mean(c)*)
      END
    END;
    m3Stop := Time.Now();

    IO.Put("m3Sum = " & Fmt.LongReal(m3Sum) & 
      " m3Time = " & Fmt.LongReal(m3Stop - m3Start) & "\n");


    fStart := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        Matrix.MulD(s.get(i),s.get(j),c);
        
(*        fSum := fSum + Matrix.Mean(c) *)
      END
    END;
    fStop := Time.Now();

    IO.Put("fSum  = " & Fmt.LongReal(fSum) & 
      " fTime  = " & Fmt.LongReal(fStop - fStart) & "\n");
  END
END Main.
