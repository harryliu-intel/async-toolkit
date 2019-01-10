INTERFACE ModelServer;
IMPORT Pathname;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT ModelServerSuper;
IMPORT UpdaterFactory;

(********************************************************************** 
 *
 *

   White Model Model Server -- for full-chip model servers 

   Main module, largely follows model_server.c from IES system in terms
   of interface.

   Author : Mika Nystrom <mika.nystroem@intel.com>
   April, 2018
                                                                      *
                                                                      *
 **********************************************************************)


CONST DefInfoFileName = "models.packetServer";

TYPE
  T <: Public;

  Public = ModelServerSuper.T OBJECT
    sharedSocket : BOOLEAN;
  METHODS
    init(sharedSocket : BOOLEAN;
         factory      : UpdaterFactory.T;
         infoPath     : Pathname.T       := ".";
         quitOnLastClientExit            := FALSE;
         infoFileName : Pathname.T       := DefInfoFileName) : T;
    (* initialize object.  infoPath is a directory path where
       the host:port file is created with the filename given below
       by InfoFileName *)
    pushPacket(READONLY hdr : FmModelMessageHdr.T; pkt : Pkt.T);
    (* for the WM to push out a packet on the wire *)

    sendPacketEot();
    (* for the WM to push a PacketEot on every writer
       (normally in this usage, there is only one writer) *)

    (****** abstract methods, implement in child type: ******)

    handlePacket(READONLY hdr : FmModelMessageHdr.T; pkt : Pkt.T);
    (* should return a handle? *)
    
  END;

CONST Brand = "ModelServer";

END ModelServer.
