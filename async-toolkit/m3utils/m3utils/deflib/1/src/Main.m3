(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;

IMPORT ProcUtils;
IMPORT Rd, FileRd;
IMPORT Debug;
IMPORT DefFormat;
IMPORT ParseParams, Params;
IMPORT Stdio;
IMPORT Pathname;
IMPORT Text;
IMPORT ParseError;

CONST TE = Text.Equal;

CONST
  Fn = "/proj/jbay/RELEASE/eth400g_mac/Feb_21_2019_07_48_13/bff/inno/dbs/eth400g_mac.output.def.gz";

CONST Usage = "";

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN
      Params.Get(0) & ": usage: " & Usage
  END DoUsage;

VAR
  c : ProcUtils.Completion;
  fn : Pathname.T := Fn;
  fRd, dRd : Rd.T;
  doUncompress : BOOLEAN;
  uncompressCommand := "gzip -dc";
BEGIN
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO

      doUncompress := pp.keywordPresent("-z");

      IF pp.keywordPresent("-Z") THEN
        uncompressCommand := pp.getNext()
      END;

      IF pp.keywordPresent("-f") THEN
        fn := pp.getNext()
      END
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  IF TE(fn, "-") THEN
    fRd := Stdio.stdin
  ELSE
    fRd := FileRd.Open(fn)
  END;

  IF doUncompress THEN
    VAR
      reader := ProcUtils.ReadHere(fRd);
      writer := ProcUtils.GimmeRd(dRd);
    BEGIN
      c := ProcUtils.RunText(uncompressCommand,
                             stdout := writer,
                             stderr := NIL,
                             stdin  := reader,
                             wd0    := NIL)
    END
  ELSE
    dRd := fRd
  END;

  TRY
    EVAL DefFormat.Parse(dRd)
  EXCEPT
    ParseError.E(x) =>
    Debug.Error("Parse error: " & x)
  END

END Main.
                    
