(* $Id$ *)

MODULE SlowTextCompress;
IMPORT Rd, Wr, UnixFilter, ProcUtils;
IMPORT TextWr, Debug, Thread;

<* FATAL Thread.Alerted *>

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

PROCEDURE Text(mode : Mode; in : TEXT) : TEXT  RAISES { ProcUtils.ErrorExit } =
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

PROCEDURE RdWr(mode : Mode; in : Rd.T; out : Wr.T) RAISES { ProcUtils.ErrorExit } =
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
    
    c.wait();

    Debug.Out("SlowTextCompress.RdWr done");
    Wr.Close(out)
  END RdWr;

BEGIN END SlowTextCompress.
