(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TestParse EXPORTS Main;
IMPORT Stdio;
IMPORT libertyLex, libertyLexStd;
IMPORT libertyParseStd;
IMPORT Debug;
IMPORT TextWr;
IMPORT Scheme;
IMPORT AL;
FROM Fmt IMPORT F, Int;
IMPORT ParseParams;
IMPORT FileRd;
IMPORT OSError;
IMPORT Rd;
IMPORT Text;
IMPORT SchemeM3;
IMPORT TextSeq;
IMPORT ReadLine, SchemeReadLine;
IMPORT SchemeStubs;
IMPORT Pathname;
IMPORT SchemeSymbol;

CONST TE = Text.Equal;

PROCEDURE GetPaths(paths : TextSeq.T) : REF ARRAY OF Pathname.T = 
  CONST
    fixed = ARRAY OF Pathname.T { "require", "m3" };
  VAR
    res := NEW(REF ARRAY OF Pathname.T, NUMBER(fixed) + paths.size());
  BEGIN
    FOR i := 0 TO NUMBER(fixed) - 1 DO
      res[i] := fixed[i]
    END;
    FOR i := NUMBER(fixed) TO paths.size() + NUMBER(fixed) - 1 DO
      res[i] := paths.remlo()
    END;
    RETURN res
  END GetPaths;

VAR
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
  scms     := NEW(TextSeq.T).init();
BEGIN
  TRY
    WHILE pp.keywordPresent("-scm") DO
      scms.addhi(pp.getNext())
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  SchemeStubs.RegisterStubs();
  TRY
    WITH scm = NEW(SchemeM3.T).init(GetPaths(scms)^) DO
      SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
    END
  EXCEPT
    Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
  END
END TestParse.
