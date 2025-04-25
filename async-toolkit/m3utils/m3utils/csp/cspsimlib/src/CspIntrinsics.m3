MODULE CspIntrinsics;
IMPORT IO;
IMPORT CspCompiledScheduler;
IMPORT CspCompiledProcess AS Process;
FROM Fmt IMPORT Unsigned, F;
IMPORT CspString;

PROCEDURE print(frame : Process.Frame; str : CspString.T) : BOOLEAN =
  BEGIN
    IO.Put(F("%s: %s: %s\n",
             Unsigned(CspCompiledScheduler.GetTime(), base := 10),
             frame.name,
             str));
    RETURN TRUE
  END print;

BEGIN END CspIntrinsics.
