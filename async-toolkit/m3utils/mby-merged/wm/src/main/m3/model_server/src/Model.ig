GENERIC INTERFACE Model(Map, MapAddr, ModelServer);
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT ModelServerSuper;

PROCEDURE HandlePacket(server :ModelServer.T;
                       READONLY read : Map.T;
                       READONLY update : MapAddr.Update;
                       READONLY hdr : FmModelMessageHdr.T;
                       pkt : Pkt.T);

PROCEDURE Setup(server : ModelServerSuper.T;
                READONLY read : Map.T;
                READONLY update : MapAddr.Update);

END Model.
