INTERFACE CspWorker;
IMPORT IP;
IMPORT Thread;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(id : CARDINAL) : T;
    getEp() : IP.Endpoint;
    getThread() : Thread.T;
    awaitInitialization();
  END;

CONST Brand = "CspWorker";

END CspWorker.
