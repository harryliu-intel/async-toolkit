INTERFACE MbyModel;
IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT MbyModelServer;

PROCEDURE HandlePacket(server : MbyModelServer.T;
                       READONLY read : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr : FmModelMessageHdr.T;
                       pkt : Pkt.T);

PROCEDURE SetupMby(server : MbyModelServer.T;
                   READONLY read : Map.T;
                   READONLY update : MapAddr.Update);

END MbyModel.
                       
