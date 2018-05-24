INTERFACE MbyModel;
IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;

PROCEDURE HandlePacket(READONLY read : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr : FmModelMessageHdr.T;
                       pkt : Pkt.T);

END MbyModel.
                       
