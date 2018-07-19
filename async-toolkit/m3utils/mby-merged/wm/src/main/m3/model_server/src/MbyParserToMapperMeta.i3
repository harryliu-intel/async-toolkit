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
  PK = MbyPaKeys.T;
  PF = MbyPaFlags.T;
  
TYPE
  T = Metadata.T OBJECT
    rxFlags            :                    MbyTypes.RxEplFlags;
    parserPktMeta      : ARRAY [0..NM-1] OF MbyTypes.Byte;
    paAdjSegLen        :                    MbyTypes.SegmentLen;
    paKeys             : ARRAY PK        OF MbyParserTypes.PaKey;
    paKeysValid        : ARRAY PK        OF BOOLEAN;
    paFlags            : ARRAY PF        OF BOOLEAN;
    paPtrs             : ARRAY [0..NP-1] OF MbyParserTypes.PaPtr;
    paPtrsValid        : ARRAY [0..NP-1] OF BOOLEAN;
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
