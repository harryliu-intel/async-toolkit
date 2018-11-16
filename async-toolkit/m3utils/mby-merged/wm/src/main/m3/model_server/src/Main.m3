MODULE Main;
IMPORT ModelServer;
IMPORT HlpModelServer, HlpModel;
IMPORT MbyModel, MbyModelServerExt;
IMPORT Pathname, Env;
IMPORT Debug;
IMPORT Scheme, SchemeStubs, SchemeNavigatorEnvironment;
IMPORT IP, ReadLineError;
IMPORT NetObj;
IMPORT AL;
IMPORT SchemeM3;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT ReadLine;
IMPORT Atom;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;
IMPORT Text;
IMPORT Thread;
IMPORT UnsafeUpdaterFactory;
IMPORT MbyModelC;

<*FATAL Thread.Alerted*>

CONST Usage = "[-ql|-quitlast] [-n[orepl]] [-m[odel] hlp|mby] [-ip|-infopath <info path>] [-if|-infofile <info filename>] [<scheme src> ...] [-reflect]";

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN Params.Get(0) & ": usage: " & Usage
  END DoUsage;

TYPE   Models     =                      {  Hlp,   Mby  };
CONST  ModelNames = ARRAY Models OF TEXT { "hlp", "mby" };
       
VAR
  modelServer : ModelServer.T;
  infoPath : Pathname.T := NIL;
  infoFile := ModelServer.DefInfoFileName;
  files : REF ARRAY OF TEXT;
  quitOnLast : BOOLEAN;
  model := Models.Hlp;
  doRepl : BOOLEAN;
  doReflect : BOOLEAN;
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-ip") OR pp.keywordPresent("-infopath") THEN
        infoPath := pp.getNext()
      ELSE
        infoPath := Env.Get("WMODEL_INFO_PATH");
      END;

      doReflect := pp.keywordPresent("-reflect");

      IF pp.keywordPresent("-if") OR pp.keywordPresent("-infofile") THEN
        infoFile := pp.getNext()
      END;

      quitOnLast := pp.keywordPresent("-ql") OR pp.keywordPresent("-quitlast");

      doRepl := NOT (pp.keywordPresent("-norepl") OR pp.keywordPresent("-n"));
      
      IF pp.keywordPresent("-model") OR pp.keywordPresent("-m") THEN
        VAR
          modelStr := pp.getNext();
          success := FALSE;
        BEGIN
          FOR i := FIRST(Models) TO LAST(Models) DO
            IF Text.Equal(modelStr, ModelNames[i]) THEN
              model := i;
              success := TRUE
            END
          END;
          IF NOT success THEN
            Debug.Error("Unknown model \"" & modelStr & "\"")
          END
        END
      END;
      
      pp.skipParsed();
      WITH nFiles = NUMBER(pp.arg^) - pp.next DO
        files := NEW(REF ARRAY OF TEXT, nFiles);
        FOR i := 0 TO nFiles-1 DO
          files[i] := pp.getNext()
        END
      END;

      pp.finish()
    END;
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  IF infoPath = NIL THEN infoPath := "." END;

  CASE model OF
    Models.Hlp =>
    modelServer := NEW(HlpModelServer.T,
                       setupChip := HlpModel.SetupHlp)
    .init(infoPath := infoPath,
          infoFileName := infoFile,
          quitOnLastClientExit := quitOnLast,
          factory := NEW(UnsafeUpdaterFactory.T).init())
  |
    Models.Mby =>
    modelServer := NEW(MbyModelServerExt.T,
                       setupChip := MbyModel.SetupMby,
                       reflect := doReflect)
    .init(infoPath := infoPath,
          infoFileName := infoFile,
          quitOnLastClientExit := quitOnLast,
          factory := MbyModelC.GetUpdaterFactory())
  END;    
    
  modelServer.resetChip();

  EVAL modelServer.listenFork();

  SchemeStubs.RegisterStubs();

  WITH arr = NEW(REF ARRAY OF Pathname.T, NUMBER(files^)+1) DO
    arr[0] := "require";
    FOR i := 1 TO LAST(arr^) DO arr[i] := files[i-1] END;
    TRY
      WITH scm = NEW(SchemeM3.T).init(arr^, 
                                      globalEnv := 
                                          NEW(SchemeNavigatorEnvironment.T).initEmpty()) DO
        scm.defineInGlobalEnv(Atom.FromText("the-server"),
                              modelServer);
        IF doRepl THEN
          MainLoop(NEW(ReadLine.Default).init(), scm)
        ELSE
          Debug.Out("Server sleeping forever.");
          WHILE TRUE DO Thread.Pause(1.0d0) END
        END
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    |
      IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
    |
      ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
    |
      NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
                                        AL.Format(err))
    END
  END
END Main.
