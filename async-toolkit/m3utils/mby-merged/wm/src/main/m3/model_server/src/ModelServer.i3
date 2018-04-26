INTERFACE ModelServer;
IMPORT Thread;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    init() : T;

    resetChip();

    listenFork() : Listener;
  END;

  Listener <: PubListener;

  PubListener = Thread.Closure OBJECT END;

CONST Brand = "ModelServer";

END ModelServer.
