MODULE Main;
IMPORT ModelServer;
IMPORT Thread;
IMPORT Pathname, Env;

VAR
  modelServer : ModelServer.T;
  infoPath : Pathname.T;
BEGIN

  infoPath := Env.Get("WMODEL_INFO_PATH");
  IF infoPath = NIL THEN infoPath := "." END;

  modelServer := NEW(ModelServer.T).init(infoPath := infoPath);

  modelServer.resetChip();

  EVAL modelServer.listenFork();

  LOOP Thread.Pause(1.0d0) END
  
END Main.
