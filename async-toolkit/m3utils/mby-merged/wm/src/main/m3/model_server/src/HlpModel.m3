MODULE HlpModel;

IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT FmModelConstants;
IMPORT HlpModelServer;
IMPORT Debug;
IMPORT ModelServer;

PROCEDURE HandlePacket(server : ModelServer.T;
                       READONLY read   : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr    : FmModelMessageHdr.T;
                       pkt             : Pkt.T) =
  VAR
    sender : Sender := NIL;
  BEGIN
    (*FloodPktHandler(hdr, pkt, sender);*)
    
    (* simplest thing ever : just reflect the packet *)
    server.pushPacket(hdr, pkt)
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

PROCEDURE SetupHlp(<*UNUSED*>server : HlpModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupHlp");
    
    update.Imn.BsmScratch3[509].Data.u(16_1109);
    (* match fpps_mgmt.c:546 *)
    
    update.Imn.FuseData[3].Data.u(16_6);
    (* match fpps_switch.c:481 *)
    
  END SetupHlp;

BEGIN END HlpModel.
