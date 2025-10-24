(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT ModelServerSuper;
IMPORT ModelServer;
IMPORT StageModelServer;

IMPORT HlpModelServer, HlpModel;
IMPORT MbyModel, MbyModelServerExt, MbyStageModelServer;

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
IMPORT UpdaterFactory, UnsafeUpdaterFactory;
IMPORT MbyModelC;
FROM Fmt IMPORT F;

<*FATAL Thread.Alerted*>

CONST Usage = "[-ql|-quitlast] [-n[orepl]] [-m[odel] hlp|mby] [-ip|-infopath <info path>] [-if|-infofile <info filename>] [-reflect] [<scheme src> ...]";

PROCEDURE DoUsage() : TEXT =
  BEGIN
    RETURN Params.Get(0) & ": usage: " & Usage
  END DoUsage;

TYPE   Model      =                         {  Hlp,   Mby  };
CONST  ModelNames = ARRAY Model OF TEXT     { "hlp", "mby" };
       ModelDefSharedSocket =
                    ARRAY Model OF BOOLEAN  { FALSE, TRUE };

PROCEDURE LookupModel(str : TEXT; VAR model : Model) : BOOLEAN =
  BEGIN
    FOR m := FIRST(Model) TO LAST(Model) DO
      IF Text.Equal(ModelNames[m], str) THEN
        model := m;
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END LookupModel;
  
VAR
  modelServer : ModelServerSuper.T;
  infoPath : Pathname.T := NIL;
  infoFile := ModelServer.DefInfoFileName;
  files : REF ARRAY OF TEXT;
  quitOnLast : BOOLEAN;
  model : Model;
  doRepl : BOOLEAN;

  doReflect, sharedSocket : BOOLEAN; (* only for fullchip *)

  stageNm : TEXT := NIL;
  (* only for stages, we use non-NIL-ness of stageNm to indicate that
     we want to simulate a stage, else we do a full-chip *)
  
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-ip") OR pp.keywordPresent("-infopath") THEN
        infoPath := pp.getNext()
      ELSE
        infoPath := Env.Get("WMODEL_INFO_PATH");
      END;

      IF pp.keywordPresent("-if") OR pp.keywordPresent("-infofile") THEN
        infoFile := pp.getNext()
      END;

      quitOnLast := pp.keywordPresent("-ql") OR pp.keywordPresent("-quitlast");

      doRepl := NOT (pp.keywordPresent("-norepl") OR pp.keywordPresent("-n"));
      
      IF pp.keywordPresent("-model") OR pp.keywordPresent("-m") THEN
        
        WITH modelStr = pp.getNext(),
             success = LookupModel(modelStr, model) DO
          IF NOT success THEN
            Debug.Error("Unknown model \"" & modelStr & "\"")
          END
        END;

        sharedSocket := ModelDefSharedSocket[model];

        IF pp.keywordPresent("-nonsharedsocket") THEN
          sharedSocket := FALSE
        ELSIF pp.keywordPresent("-sharedsocket") THEN
          sharedSocket := TRUE
        END;

        doReflect := pp.keywordPresent("-reflect");

      ELSIF pp.keywordPresent("-stage") OR pp.keywordPresent("-s") THEN
        WITH modelStr = pp.getNext(),
             success = LookupModel(modelStr, model) DO
          IF NOT success THEN
            Debug.Error("Unknown model \"" & modelStr & "\"")
          END
        END;
        stageNm := pp.getNext();
        Debug.Out(F("Seeking model %s stage %s", ModelNames[model], stageNm));
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

  VAR
    factory : UpdaterFactory.T;
  BEGIN
    CASE model OF
      Model.Hlp =>
      <*ASSERT stageNm = NIL*> (* stages not supported *)
      modelServer := NEW(HlpModelServer.T, setup := HlpModel.Setup);
      factory := NEW(UnsafeUpdaterFactory.T).init()
    |
      Model.Mby =>

      IF stageNm = NIL THEN
        modelServer := NEW(MbyModelServerExt.T,
                           setup := MbyModel.Setup,
                           reflect := doReflect)
      ELSE
        modelServer := NEW(MbyStageModelServer.T,
                           setup := MbyModel.Setup)
      END;
      factory := MbyModelC.GetUpdaterFactory()
    END;

    IF stageNm = NIL THEN
      WITH ms = NARROW(modelServer, ModelServer.T) DO
        (* "full-chip" model server *)
        EVAL ms.init(sharedSocket,
                     infoPath             := infoPath,
                     infoFileName         := infoFile,
                     quitOnLastClientExit := quitOnLast,
                     factory              := factory)
      END
    ELSE
      WITH ms = NARROW(modelServer, StageModelServer.T) DO
        (* single stage model server *)
        EVAL ms.init(stageNm,
                     infoPath             := infoPath,
                     infoFileName         := infoFile,
                     quitOnLastClientExit := quitOnLast,
                     factory              := factory)
      END
    END
  END;
    
  modelServer.reset();

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
