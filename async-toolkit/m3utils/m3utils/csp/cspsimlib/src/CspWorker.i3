INTERFACE CspWorker;
IMPORT IP;
IMPORT Thread;
IMPORT CspSim;
IMPORT TextCardTbl;
IMPORT TextRemoteChannelTbl;
IMPORT CspRemote;

TYPE
  T <: Public;

  Public = CspRemote.T OBJECT METHODS
    init(id : CARDINAL; bld : CspSim.Builder) : T;

    getEp() : IP.Endpoint;

    getThread() : Thread.T;

    awaitInitialization();
    (* network is initialized *)

    getProcMap() : TextCardTbl.T;
    (* mapping of processes to scheduler id *)

    getChannelData() : TextRemoteChannelTbl.T;

    getId() : CARDINAL;
  END;

CONST Brand = "CspWorker";

END CspWorker.
