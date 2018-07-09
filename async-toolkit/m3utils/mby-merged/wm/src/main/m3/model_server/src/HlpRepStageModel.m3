MODULE HlpRepStageModel;

IMPORT hlp_top_map_addr AS TopAddr;

IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT Coroutine;
IMPORT BaseModelStage;
IMPORT ModelStageResult;

PROCEDURE HandlePacket(from : Coroutine.T;
                       ipkt : Pkt.T;
                       h : TopAddr.H;
                       indices : Indices;
                       imd : Metadata.T;
                       out : ModelStageResult.T) =
  BEGIN
    (* purpose of this routine is to map the records *)
    HandlePacketInt(from,
                    ipkt,
                    h.read ,
                    h.update,
                    imd,
                    out)
  END HandlePacket;
  
PROCEDURE HandlePacketInt(from        : Coroutine.T;
                       ipkt        : Pkt.T;
                       READONLY r  : Map.T;
                       READONLY u  : MapAddr.U;
                       im          : Metadata.T;
                       out         : ModelStageResult.T) =
  CONST
    Copies = 5;
  BEGIN
    (* duplicate, just to test *)
    FOR i := 1 TO Copies DO
      out.opkt := ipkt;
      out.om := NEW(Metadata.T);
      EVAL Coroutine.Call(from);
    END;
    out.opkt := NIL (* signal EOT *)
  END HandlePacketInt;

BEGIN END HlpRepStageModel.
