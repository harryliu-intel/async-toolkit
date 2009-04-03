(* $Id$ *)

INTERFACE Example;

TYPE
  T = OBJECT METHODS
    hello();
    goodbye(s1 : S; s2 : REF W; s3 : W; s4 : INTEGER) : V;
  END;

  R = RECORD
    first : LONGREAL;
    second : INTEGER;
  END;

  Q = [1900..2009];

  S = { One, Two, Three, Four };
  
  U = [S.Two .. S.Three];

  V = SET OF U;

  W = INTEGER;

  B = BOOLEAN;

  C = CHAR;

  M = MUTEX;

CONST True = BOOLEAN.TRUE;

CONST Brand = "Example";

PROCEDURE TakesS(s : S := S.Two);

CONST PP = TakesS;

TYPE PType = PROCEDURE (s : S);

CONST AnS = S.Two;

END Example.
