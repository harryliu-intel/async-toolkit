INTERFACE ModelServer;
IMPORT Thread;
IMPORT Pathname;
IMPORT CsrOp, CsrAccessStatus;

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    init(infoPath : Pathname.T := ".") : T;

    listenFork() : Listener;

    (* abstract methods, implement in child type: *)
    resetChip();

    csrOp(VAR op : CsrOp.T) : CsrAccessStatus.T;
  END;

  Listener <: PubListener;

  PubListener = Thread.Closure OBJECT END;

CONST Brand = "ModelServer";

CONST InfoFileName = "models.packetServer";

EXCEPTION ParseError(TEXT);
          
END ModelServer.
