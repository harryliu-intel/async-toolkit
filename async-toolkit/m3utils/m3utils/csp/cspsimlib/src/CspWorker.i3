INTERFACE CspWorker;
IMPORT IP;
IMPORT Thread;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(id : CARDINAL) : T;
    getEp() : IP.Endpoint;
    getThread() : Thread.T;
  END;

CONST Brand = "CspWorker";

END CspWorker.
