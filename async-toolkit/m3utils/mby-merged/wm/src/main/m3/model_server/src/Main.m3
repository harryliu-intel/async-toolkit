MODULE Main;
IMPORT HlpModelServer;
IMPORT Pathname, Env;
IMPORT hlp_top_map      AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT Debug;
IMPORT Scheme, SchemeStubs, SchemeNavigatorEnvironment;
IMPORT IP, ReadLineError;
IMPORT OSError;
IMPORT NetObj;
IMPORT AL;
IMPORT SchemeM3;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT ReadLine;
IMPORT Atom;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Params;

CONST Usage = "[-ql|-quitlast] [-ip|-infopath <info path>] [<scheme src> ...]";

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN Params.Get(0) & ": usage: " & Usage
  END DoUsage;

PROCEDURE SetupHlp(<*UNUSED*>server : HlpModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupHlp");
    
    update.Imn.BsmScratch3[509].Data.u(16_1109);
    (* match fpps_mgmt.c:546 *)
    
    update.Imn.FuseData[3].Data.u(16_6);
    (* match fpps_switch.c:481 *)
    
  END SetupHlp;

VAR
  modelServer : HlpModelServer.T;
  infoPath : Pathname.T := NIL;
  files : REF ARRAY OF TEXT;
  quitOnLast : BOOLEAN;
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-ip") OR pp.keywordPresent("-infopath") THEN
        infoPath := pp.getNext()
      ELSE
        infoPath := Env.Get("WMODEL_INFO_PATH");
      END;

      quitOnLast := pp.keywordPresent("-ql") OR pp.keywordPresent("-quitlast");
      
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

  modelServer := NEW(HlpModelServer.T,
                     setupChip := SetupHlp)
    .init(infoPath := infoPath,
          quitOnLastClientExit := quitOnLast);

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
