INTERFACE HlpModel;
IMPORT HlpModelServer;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT ModelServer;

PROCEDURE HandlePacket(server : ModelServer.T;
                       READONLY read : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr : FmModelMessageHdr.T;
                       pkt : Pkt.T);

PROCEDURE SetupHlp(server : HlpModelServer.T;
                   READONLY read : Map.T;
                   READONLY update : MapAddr.Update);

END HlpModel.
                       
