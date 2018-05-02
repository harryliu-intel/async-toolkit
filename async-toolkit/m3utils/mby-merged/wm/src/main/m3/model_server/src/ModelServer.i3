INTERFACE ModelServer;
IMPORT Thread;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    init(infoPath : Pathname.T := ".") : T;

    resetChip();

    listenFork() : Listener;
  END;

  Listener <: PubListener;

  PubListener = Thread.Closure OBJECT END;

CONST Brand = "ModelServer";

CONST InfoFileName = "models.packetServer";

END ModelServer.
