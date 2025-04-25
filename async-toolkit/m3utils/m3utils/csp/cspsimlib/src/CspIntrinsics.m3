MODULE CspIntrinsics;
IMPORT IO;
IMPORT CspCompiledScheduler;
FROM Fmt IMPORT Unsigned, F;
IMPORT CspString;

PROCEDURE print(str : CspString.T) : BOOLEAN =
  BEGIN
    IO.Put(F("%s: %s\n",
             Unsigned(CspCompiledScheduler.GetTime(), base := 10),
             str));
    RETURN TRUE
  END print;

BEGIN END CspIntrinsics.
