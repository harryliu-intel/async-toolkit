MODULE Main;
IMPORT FileRd;
IMPORT Rd, Text, Name, NameNameTbl, NameList, NameNameListTbl;
IMPORT IO, Fmt, Process;
IMPORT ParseParams, Stdio, Pathname;

IMPORT Thread;
IMPORT SchemeM3;
IMPORT SchemeNavigatorEnvironment;
IMPORT SchemeStubs;
IMPORT Params, Scheme, IP, AL, ReadLine;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT Debug, OSError, ReadLineError, NetObj;
IMPORT Dsim;
IMPORT NameRefTbl;
IMPORT NameSet, NameSetDef;
IMPORT TextUtils;
IMPORT Wr;
IMPORT SchemeSymbol, SchemeString;

<*FATAL Thread.Alerted*>
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
BEGIN
  SchemeStubs.RegisterStubs();

  IO.Put("pp.next=" & Fmt.Int(pp.next) & " Params.Count=" & Fmt.Int(Params.Count) & "\n");

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count-pp.next+1) DO
    arr[0] := "require";
    FOR i := pp.next TO Params.Count-1 DO arr[i-pp.next+1] := Params.Get(i) END;
    TRY
      WITH scm = NEW(SchemeM3.T).init(ARRAY OF Pathname.T { "require" },
                                      globalEnv := 
                                NEW(SchemeNavigatorEnvironment.T).initEmpty()) DO
        FOR i := 1 TO LAST(arr^) DO
          EVAL scm.loadFile(SchemeString.FromText(arr[i]))
        END;


        MainLoop(NEW(ReadLine.Default).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    |
      IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
    |
      OSError.E(err) => 
        Debug.Error("Caught NetObj.Error : " & AL.Format(err))
    |
      ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
    |
      NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
                                        AL.Format(err))
    END
  END

END Main.
