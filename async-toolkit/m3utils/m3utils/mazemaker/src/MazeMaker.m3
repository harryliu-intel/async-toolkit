MODULE MazeMaker EXPORTS Main;
IMPORT IO, Fmt, Params, Scan;

TYPE
  P = RECORD x, y : INTEGER END;

PROCEDURE U(t : TEXT) =
  BEGIN IO.Put(t & " ") END U;

PROCEDURE CR() = BEGIN IO.Put("\n") END CR;

TYPE
  Mode = { Maze, Empty };
  
VAR
  rs := Scan.Int(Params.Get(1));
  cs := Scan.Int(Params.Get(2));
  mode := Mode.Empty;
BEGIN
  IO.Put(Fmt.Int(rs)); CR();
  IO.Put(Fmt.Int(cs)); CR();
  
  FOR r := 0 TO rs-1 DO
    FOR c := 0 TO cs-1 DO
      IF P { r, c } = P { 0, 0 } THEN
        U("s")
      ELSIF r = rs-1 AND 
        ( (r MOD 4 = 0 AND c = cs-1)  OR
          (r MOD 4 = 1 AND c = cs-1) OR
          (r MOD 4 = 2 AND c = 0) OR
          (r MOD 4 = 3 AND c = 0) ) THEN
        U("g")
      ELSIF mode = Mode.Maze AND r MOD 4 = 1 AND c # cs-1 THEN
        U("x")
      ELSIF mode = Mode.Maze AND r MOD 4 = 3 AND c # 0 THEN
        U("x")
      ELSE 
        U("_")
      END
    END;
    CR()
  END
END MazeMaker.
