MODULE HlpRepStageModel;

IMPORT hlp_top_map_addr AS TopAddr;

IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT ModelStageResult;

PROCEDURE HandlePacket(ipkt    : Pkt.T;
                       h       : TopAddr.H;
                       indices : Indices;
                       imd     : Metadata.T;
                       out     : ModelStageResult.T) =
  BEGIN
    (* purpose of this routine is to map the records *)
    HandlePacketInt(ipkt,
                    h.read ,
                    h.update,
                    imd,
                    out)
  END HandlePacket;
  
PROCEDURE HandlePacketInt(ipkt        : Pkt.T;
                          READONLY r  : Map.T;
                          READONLY u  : MapAddr.U;
                          im          : Metadata.T;
                          out         : ModelStageResult.T) =
  CONST
    Copies = 5;
  BEGIN
    (* duplicate, just to test *)
    FOR i := 1 TO Copies DO
      out.push(opkt := ipkt, om := NEW(Metadata.T))
    END
  END HandlePacketInt;

BEGIN END HlpRepStageModel.
