MODULE CspIntrinsics;
IMPORT IO;
IMPORT CspCompiledScheduler;
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT Frame;
FROM Fmt IMPORT Unsigned, F, Int;
IMPORT Mpz;
IMPORT CspIntrinsicsP AS P;

IMPORT CspString;
IMPORT NativeInt;
IMPORT DynamicInt;

IMPORT Debug;

PROCEDURE print(frame : Process.Frame; str : CspString.T) : BOOLEAN =
  BEGIN
    IO.Put(F("%s: %s: %s\n",
             Unsigned(CspCompiledScheduler.GetTime(), base := 10),
             frame.name,
             str));
    RETURN TRUE
  END print;
  
PROCEDURE string_native(<*UNUSED*>frame  : Frame;
                        num              : NativeInt.T;
                        base             : INTEGER) : TEXT =
  BEGIN
    RETURN Int(num, base := base)
  END string_native;

PROCEDURE string_dynamic(<*UNUSED*>frame : Frame;
                         num             : DynamicInt.T;
                         base            : INTEGER) : TEXT =
  BEGIN
    RETURN Mpz.FormatBased(num, base)
  END string_dynamic;

PROCEDURE walltime(<*UNUSED*>frame : Frame) : NativeInt.T =
  BEGIN
    RETURN P.GetNanoclock()
  END walltime;

PROCEDURE simtime(<*UNUSED*>frame : Frame) : NativeInt.T =
  BEGIN RETURN CspCompiledScheduler.GetTime() END simtime;

PROCEDURE assert(x : BOOLEAN; text : TEXT) =
  BEGIN
    IF NOT x THEN
      Debug.Error("Assertion failed : " & text)
    END
  END assert;
  
BEGIN END CspIntrinsics.
