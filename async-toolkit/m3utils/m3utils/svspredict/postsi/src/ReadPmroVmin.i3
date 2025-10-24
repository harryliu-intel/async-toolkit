INTERFACE ReadPmroVmin;
IMPORT Lex, FloatMode, Rd;
IMPORT PMRO;

PROCEDURE Read(rd : Rd.T;
               Unit : LONGREAL; (* unit of Vmin input *)
               READONLY pmro : ARRAY OF PMRO.T; (* PMRO defs *)
               VAR n : CARDINAL;
               VAR meanVmin, sdevVmin : LONGREAL;
               VAR meanPmro, sdevPmro : LONGREAL)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure };

END ReadPmroVmin.
