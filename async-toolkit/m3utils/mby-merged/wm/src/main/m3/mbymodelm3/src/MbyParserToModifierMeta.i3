INTERFACE MbyParserToModifierMeta;
IMPORT Metadata;
IMPORT MbyParserInfo;

TYPE
  T = Metadata.T OBJECT
    info : MbyParserInfo.T;
  END;

CONST Brand = "MbyParserToModifierMeta";

END MbyParserToModifierMeta.
