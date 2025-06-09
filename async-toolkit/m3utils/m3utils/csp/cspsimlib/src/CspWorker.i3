INTERFACE CspWorker;
IMPORT IP;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    getEp() : IP.Endpoint;
  END;

CONST Brand = "CspWorker";

END CspWorker.
