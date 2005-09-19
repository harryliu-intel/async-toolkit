(* $Id$ *)

MODULE SlowTextCompress;
IMPORT Rd, Wr, UnixFilter, ProcUtils;
IMPORT TextRd, TextWr;

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

BEGIN END SlowTextCompress.
