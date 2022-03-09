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

PROCEDURE GetPaths(extras : TextSeq.T) : REF ARRAY OF Pathname.T = 
  CONST
    fixed = ARRAY OF Pathname.T { "require", "m3" };
  VAR
    res := NEW(REF ARRAY OF Pathname.T, NUMBER(fixed) + extras.size());
  BEGIN
    FOR i := 0 TO NUMBER(fixed) - 1 DO
      res[i] := fixed[i]
    END;
    FOR i := NUMBER(fixed) TO extras.size() + NUMBER(fixed) - 1 DO
      res[i] := extras.remlo()
    END;
    RETURN res
  END GetPaths;

VAR
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
  extra    := NEW(TextSeq.T).init();
  rd       : Rd.T;
  lexer    := NEW(libertyLexStd.T);
  parser   := NEW(libertyParseStd.T);
  doScheme := FALSE;
BEGIN
  TRY
    doScheme := pp.keywordPresent("-scm");
    pp.skipParsed();
    WITH n = NUMBER(pp.arg^) - pp.next DO
      FOR i := 0 TO n - 1 DO
        extra.addhi(pp.getNext())
      END
    END;
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF extra.size() = 0 THEN
    Debug.Error("?must provide path (or \"-\" for std. input) of input as arg 1.")
  END;

  Debug.Out("extra.size " & Int(extra.size()));
  
  VAR
    path := extra.remhi(); (* last arg is lib file *)
  BEGIN
    IF TE(path, "-") THEN
      rd := Stdio.stdin
    ELSE
      TRY
        rd := FileRd.Open(path)
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("?couldn't open input %s : OSError.E : %s",
                      path,
                      AL.Format(x)))
      END
    END
  END;
  
  EVAL lexer.setRd(rd);
  EVAL parser.setLex(lexer);

  EVAL parser.parse();

  WITH wr = NEW(TextWr.T).init() DO
    parser.val.write(wr);
    Debug.Out("Parsed:\n" & TextWr.ToText(wr))
  END;

  IF doScheme THEN
    SchemeStubs.RegisterStubs();
    TRY
      WITH scm = NEW(SchemeM3.T).init(GetPaths(extra)^) DO
        scm.defineInGlobalEnv(SchemeSymbol.FromText("*lib*"), parser.val);
        SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    END
  END

  
END TestParse.
