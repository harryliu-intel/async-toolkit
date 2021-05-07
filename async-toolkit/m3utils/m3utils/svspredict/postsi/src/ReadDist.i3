INTERFACE ReadDist;
IMPORT Lex, FloatMode, Rd;

PROCEDURE Read(rd : Rd.T;
               Unit : LONGREAL; (* unit of input *)
               VAR n : CARDINAL;
               VAR mean, sdev : LONGREAL;
               VAR title : TEXT)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure };

END ReadDist.
