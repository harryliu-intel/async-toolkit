(* $Id$ *)

MODULE SlowTextCompress;
IMPORT Rd, Wr, UnixFilter, ProcUtils;
IMPORT TextRd, TextWr, Debug;
IMPORT Process, Fmt;

CONST Command = ARRAY Mode OF TEXT { "bzip2 -cz", "bzip2 -cd" };

PROCEDURE RR(mode : Mode; source : Rd.T) : Rd.T =
  BEGIN
    RETURN UnixFilter.RR(Command[mode],source)
  END RR;

PROCEDURE WW(mode : Mode; target : Wr.T) : Wr.T =
  BEGIN
    RETURN UnixFilter.WW(Command[mode],target)
  END WW;

PROCEDURE RW(mode : Mode; source : Rd.T; target : Wr.T) = 
  BEGIN
    UnixFilter.RW(Command[mode],source, target)
  END RW;

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT =
  VAR
    wrIn : Wr.T;
    wr := NEW(TextWr.T).init();
    writer := ProcUtils.WriteHere(wr);
    c : ProcUtils.Completion;
  BEGIN
    c := ProcUtils.RunText(Command[mode], stdout := writer, 
                           stdin := ProcUtils.GimmeWr(wrIn));

    Wr.PutText(wrIn, in);
    Wr.Close(wrIn);

    c.wait();
    RETURN TextWr.ToText(wr)
  END Text;

PROCEDURE RdWr(mode : Mode; in : Rd.T; out : Wr.T) =
  VAR
    writer := ProcUtils.WriteHere(out);
    wrIn : Wr.T;
    c : ProcUtils.Completion;
  BEGIN
    Debug.Out("SlowTextCompress.RdWr starting");
    c := ProcUtils.RunText(Command[mode], 
                           stdout := writer, 
                           stdin := ProcUtils.GimmeWr(wrIn));

    TRY
      LOOP
        WITH c = Rd.GetChar(in) DO Wr.PutChar(wrIn,c) END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(in); Wr.Close(wrIn)
    END;
    
    TRY
      c.wait()
    EXCEPT
      ProcUtils.ErrorExit(e) =>
      TYPECASE(e) OF
        ProcUtils.ExitCode(ec) => Process.Crash(Command[mode] & 
          " exited with error code " & Fmt.Int(ec.code))
      |
        ProcUtils.OS(os) => Process.Crash("Caught error " & os.error)
      END
    END;

    Debug.Out("SlowTextCompress.RdWr done");
    Wr.Close(out)
  END RdWr;

BEGIN END SlowTextCompress.
