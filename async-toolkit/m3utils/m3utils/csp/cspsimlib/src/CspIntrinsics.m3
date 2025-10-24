MODULE CspIntrinsics;
FROM CspScheduler IMPORT GetTime;
IMPORT CspCompiledProcess AS Process;
FROM CspCompiledProcess IMPORT Frame;
FROM Fmt IMPORT Unsigned, F, Int;
IMPORT Mpz;
IMPORT CspIntrinsicsP AS P;

IMPORT CspString;
IMPORT NativeInt;
IMPORT DynamicInt;

IMPORT Debug;
IMPORT Random;
IMPORT Word;
IMPORT Wr;
IMPORT Stdio;
IMPORT Thread;

<*FATAL Thread.Alerted*>

PROCEDURE DefaultPut(<*UNUSED*>self : Putter; str : CspString.T) =
  BEGIN
    Wr.PutText(Stdio.stdout, str);
    Wr.Flush(Stdio.stdout)
  END DefaultPut;

VAR putstring : Putter := NEW(Putter, put := DefaultPut);
    
PROCEDURE print(frame : Process.Frame; str : CspString.T) : BOOLEAN =
  BEGIN
    putstring.put(F("%s: %s: %s\n",
                    Unsigned(GetTime(), base := 10),
                    frame.name,
                    str));
    RETURN TRUE
  END print;

PROCEDURE GetPutter() : Putter = BEGIN RETURN putstring END GetPutter;

PROCEDURE SetPutter(putter : Putter) = BEGIN putstring := putter END SetPutter;
  
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
  BEGIN RETURN GetTime() END simtime;

PROCEDURE assert(x : BOOLEAN; text : TEXT) : NativeInt.T =
  BEGIN
    IF NOT x THEN
      Debug.Error("Assertion failed : " & text)
    END;
    RETURN 0
  END assert;

PROCEDURE random_native(bits : NativeInt.T) : NativeInt.T =
  VAR
    x := rand.integer();
  BEGIN
    IF bits = BITSIZE(Word.T) - 1 THEN
      RETURN Word.And(x, NativeInt.Max)
    ELSE
      WITH mask = Word.Shift(1, bits) - 1 DO
        RETURN Word.And(x, mask)
      END
    END
  END random_native;

PROCEDURE random_dynamic(x : DynamicInt.T; bits : NativeInt.T) : DynamicInt.T =
  BEGIN
    Mpz.set_ui(x, 0);
    WHILE bits # 0 DO
      WITH b = MIN(BITSIZE(Word.T) - 1, bits) DO
        Mpz.LeftShift(x, x, b);
        Mpz.add_ui(x, x, random_native(b));
        DEC(bits, b)
      END
    END;
    RETURN x
  END random_dynamic;

VAR
  rand := NEW(Random.Default).init(TRUE);
BEGIN END CspIntrinsics.
