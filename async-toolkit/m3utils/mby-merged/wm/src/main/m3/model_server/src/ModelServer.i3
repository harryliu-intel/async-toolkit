INTERFACE ModelServer;
IMPORT Thread;
IMPORT Pathname;
IMPORT CsrOp, CsrAccessStatus;

(* 
   White Model Model Server 

   Main module, largely follows model_server.c from IES system in terms
   of interface.

   Author : Mika Nystrom <mika.nystroem@intel.com>
   April, 2018
*)

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
