INTERFACE MbyMacToParser;
IMPORT MbyTypes;

TYPE
  T = RECORD
    rxLength : MbyTypes.PacketLen;
    rxPort   : MbyTypes.Port;
  END;

CONST Brand = "MbyMacToParser";

END MbyMacToParser.
