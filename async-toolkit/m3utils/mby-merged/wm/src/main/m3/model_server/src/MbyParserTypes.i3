INTERFACE MbyParserTypes;

TYPE
  PaKey               = [0..16_ffff];
  PaPtr               = [0..16_ff];
  PaCsumOk            = [0..16_ff]; (* is this right? *)
  PaExStage           = [0..16_ff]; (* is this right? *)
  PaPacketType        = [0..16_ff]; (* is this right? *)

CONST Brand = "MbyParserTypes";

END MbyParserTypes.
