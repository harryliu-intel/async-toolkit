INTERFACE MbyParserToMapperMeta;
IMPORT Metadata;
IMPORT MbyTypes;
IMPORT MbyParserTypes;
IMPORT MbyParserSizes;
IMPORT MbyPaKeys;
IMPORT MbyPaFlags;

(* from wm/src/main/c/mby_parser.h *)

CONST
  NM = MbyParserSizes.NMeta;
  NK = MbyParserSizes.NKeys;
  NF = MbyParserSizes.NFlags;
  NP = MbyParserSizes.NPtrs;

TYPE
  PF = MbyPaFlags.T;
  
TYPE
  PaKeys = ARRAY [0..NK-1] OF MbyParserTypes.PaKey;
  PaPtrs = ARRAY [0..NP-1] OF MbyParserTypes.PaPtr;
  
  T = Metadata.T OBJECT
    rxFlags            :                    MbyTypes.RxEplFlags;
    parserPktMeta      : ARRAY [0..NM-1] OF MbyTypes.Byte;  (* ??? does this belong here ??? *)
    paAdjSegLen        :                    MbyTypes.SegmentLen;
    paKeys             :                    PaKeys;
    paFlags            : ARRAY PF        OF BOOLEAN;
    paPtrs             :                    PaPtrs;
    paCsumOk           :                    MbyParserTypes.PaCsumOk;
    paExStage          :                    MbyParserTypes.PaExStage;
    paExDepthExceed    :                    BOOLEAN;
    paExTruncHeader    :                    BOOLEAN;
    paExParsingDone    :                    BOOLEAN;
    paDrop             :                    BOOLEAN;
    paL3lenErr         :                    BOOLEAN;
    paPacketType       :                    MbyParserTypes.PaPacketType;
  END;

CONST Brand = "MbyParserToMapperMeta";

END MbyParserToMapperMeta.
