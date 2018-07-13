INTERFACE MbyMacToParserMeta;
IMPORT Metadata;
IMPORT MbyTypes;

TYPE
  T = Metadata.T OBJECT
    rxLength : MbyTypes.PacketLen;
    rxPort   : MbyTypes.Port;
  END;

CONST Brand = "MbyMacToParserMeta";

END MbyMacToParserMeta.
