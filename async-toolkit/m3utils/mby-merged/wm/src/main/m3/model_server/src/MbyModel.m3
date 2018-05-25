MODULE MbyModel;

IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT FmModelConstants;
IMPORT MbyModelServer;
IMPORT Debug;

PROCEDURE HandlePacket(READONLY read   : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr    : FmModelMessageHdr.T;
                       pkt             : Pkt.T) =
  VAR
    sender : Sender := NIL;
  BEGIN
    FloodPktHandler(hdr, pkt, sender);
  END HandlePacket;

TYPE Sender = PROCEDURE(READONLY hHdr : FmModelMessageHdr.T;
                        ePkt : Pkt.T);

PROCEDURE FloodPktHandler(READONLY hdr : FmModelMessageHdr.T;
                          pkt          : Pkt.T;
                          sender       : Sender) =
  BEGIN
    FOR i := 0 TO FmModelConstants.NPhysPorts-1 DO
      IF i # hdr.port THEN
        (* send to all ports except ingress port *)
        VAR
          eHdr := hdr;
        BEGIN
          eHdr.port := i;
          sender(eHdr, pkt)
        END
      END
    END
  END FloodPktHandler;

PROCEDURE SetupMby(<*UNUSED*>server : MbyModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupMby");
    
    (* fill this in *)
  END SetupMby;

BEGIN END MbyModel.
