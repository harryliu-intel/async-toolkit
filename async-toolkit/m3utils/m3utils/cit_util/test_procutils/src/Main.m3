(*
   Copyright (c) 2010 Generation Capital Ltd.
   All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

(* $Id$ *)

MODULE Main;
IMPORT SlowTextCompress;
IMPORT FileRd, Stdio, Thread, FileWr, Process, ProcUtils;

VAR
  rd := FileRd.Open("in.bz2");
  wr := FileWr.Open("out");
BEGIN
  TRY
    SlowTextCompress.RdWr(SlowTextCompress.Mode.Decompress,rd,wr);
  EXCEPT
    ProcUtils.ErrorExit(e) =>         
       Process.Crash("ProcUtils.ErrorExit in UnStuffPickle: " &
                      ProcUtils.FormatError(e))
  END;

  LOOP Thread.Pause(1.0d0) END
END Main.
