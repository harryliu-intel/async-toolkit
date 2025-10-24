MODULE Main;
IMPORT Params;
IMPORT Scan;
IMPORT FileNamer;
IMPORT TempReader;
IMPORT RegularFile, FS;
IMPORT FileWr, Wr;
FROM Fmt IMPORT Int, F;
IMPORT AL;
IMPORT OSError;
IMPORT Debug;
IMPORT Thread;
IMPORT TraceFile;
IMPORT Rd;
IMPORT Lex, FloatMode;

(* $0 <wd> <dataStartByte> <nSteps> <nFiles> <nNames> <lo> <hi> <trace-fn> *)

<*FATAL Thread.Alerted*>

VAR
  idx : CARDINAL := LAST(CARDINAL);
BEGIN
  TRY
    VAR
      wd            :=          Params.Get(1);
      dataStartByte := Scan.Int(Params.Get(2));
      nSteps        := Scan.Int(Params.Get(3));
      nFiles        := Scan.Int(Params.Get(4));
      nNames        := Scan.Int(Params.Get(5));
      lo            := Scan.Int(Params.Get(6));
      hi            := Scan.Int(Params.Get(7));
      tFn           :=          Params.Get(8);

      (* make all the stuff we need *)
      buff := NEW(REF ARRAY OF LONGREAL, nSteps);
      fnr  := NEW(FileNamer.T).init(wd, nFiles, nNames);
      trd  := NEW(TempReader.T).init(fnr);
      file : RegularFile.T := FS.OpenFile(tFn, truncate := FALSE);
      wr   := NEW(FileWr.T).init(file);
    BEGIN

      Wr.Seek(wr, 0);

      FOR i := lo TO hi DO
        idx := i;
        TraceFile.BlockWrite(wr,
                             trd,
                             i,
                             dataStartByte,
                             buff^)
      END;

      Wr.Flush(wr);

      Wr.Close(wr)
    END
  EXCEPT
    Wr.Failure(x) => Debug.Error(F("I/O error writing lately processing signal %s: Wr.Failure : %s", Int(idx), AL.Format(x)))
  |
    Rd.Failure(x) => Debug.Error(F("I/O error reading lately processing signal %s: Rd.Failure : %s", Int(idx), AL.Format(x)))
  |
    OSError.E(x) => Debug.Error(F("Error opening files for conversion : OSError.E : %s", AL.Format(x)))
  |
    FloatMode.Trap, Lex.Error => Debug.Error("Can't parse command-line params.")
  END
END Main.
