INTERFACE ModelServer;
IMPORT Thread;
IMPORT Pathname;
IMPORT CsrOp, CsrAccessStatus;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;

(********************************************************************** 

   White Model Model Server 

   Main module, largely follows model_server.c from IES system in terms
   of interface.

   Author : Mika Nystrom <mika.nystroem@intel.com>
   April, 2018

 **********************************************************************)

TYPE
  T <: Public;

  Public = OBJECT
  METHODS
    init(infoPath : Pathname.T := "."; quitOnLastClientExit := FALSE) : T;
    (* initialize object.  infoPath is a directory path where
       the host:port file is created with the filename given below
       by InfoFileName *)

    listenFork() : Listener;
    (* fork a listener on an arbitrarily chosen port *)

    (* abstract methods, implement in child type: *)
    resetChip();

    csrOp(VAR op : CsrOp.T) : CsrAccessStatus.T;
    (* perform a CSR operation as requested.
       if a read, the read results are returned in the op itself. *)

    handlePacket(READONLY hdr : FmModelMessageHdr.T; pkt : Pkt.T);
    (* should return a handle? *)
    
  END;

  Listener <: PubListener;

  PubListener = Thread.Closure OBJECT END;

CONST Brand = "ModelServer";

CONST InfoFileName = "models.packetServer";

EXCEPTION ParseError(TEXT);
          
END ModelServer.
