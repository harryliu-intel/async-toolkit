INTERFACE MbyParserTypes;
IMPORT Word;

CONST LS = Word.LeftShift;
      
TYPE
  PaKey               = [0..LS(1,16)-1];
  PaPtr               = [0..LS(1,8)-1];
  PaCsumOk            = [0..LS(1,8)-1]; (* is this right? *)
  PaExStage           = [0..LS(1,8)-1]; (* is this right? *)
  PaPacketType        = [0..LS(1,8)-1]; (* is this right? *)

  L2Len               = [0..LS(1,3)-1];
  L3Len               = [0..LS(1,4)-1];
  MplsLen             = [0..LS(1,3)-1];
  TunLen              = [0..LS(1,5)-1];

CONST Brand = "MbyParserTypes";

END MbyParserTypes.
