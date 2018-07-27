UNSAFE MODULE MbyModelC EXPORTS MbyModel;

IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT FmModelConstants;
IMPORT MbyModelServer;
IMPORT Debug;
IMPORT MbyParserStage;
IMPORT BaseModelStage;
IMPORT Metadata;
IMPORT MbyParserStageModel;
IMPORT mby_top_map_c;
IMPORT Fmt; FROM Fmt IMPORT Int, F;
IMPORT Word;
IMPORT IntSeq AS AddrSeq;

PROCEDURE HandlePacket(server           : MbyModelServer.T;
                       READONLY readA   : Map.T;
                       READONLY updateA : MapAddr.Update;
                       READONLY hdr     : FmModelMessageHdr.T;
                       pkt              : Pkt.T) =
  BEGIN
  END HandlePacket;

PROCEDURE SetupMby(<*UNUSED*>server : MbyModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupMby");
    mby_top_map_c.BuildMain(BuildCallback);
    Debug.Out(F("Mapped %s fields in C struct",Int(addrSeq.size())));
  END SetupMby;

VAR
  addrSeq := NEW(AddrSeq.T).init();
  
PROCEDURE BuildCallback(addr : ADDRESS) =
  BEGIN
    WITH w = LOOPHOLE(addr,Word.T) DO
      IF FALSE THEN
        Debug.Out("BuildCallback " & Fmt.Unsigned(w))
      END;
      addrSeq.addhi(w)
    END
  END BuildCallback;
  
BEGIN END MbyModelC.
