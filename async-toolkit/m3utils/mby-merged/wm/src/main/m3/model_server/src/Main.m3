MODULE Main;
IMPORT ModelServer;
IMPORT Thread;

VAR
  modelServer : ModelServer.T;

BEGIN

  modelServer := NEW(ModelServer.T).init();

  modelServer.resetChip();

  EVAL modelServer.listenFork();

  LOOP Thread.Pause(1.0d0) END
  
END Main.
