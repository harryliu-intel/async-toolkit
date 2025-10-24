INTERFACE BaseModelStage;
IMPORT Metadata;
IMPORT ServerPacket AS Pkt;
IMPORT Coroutine;

TYPE
  T = OBJECT
  METHODS
    poll (VAR out : Pkt.T; VAR meta : Metadata.T) : BOOLEAN;
  END;

CONST Brand = "BaseModelStage";

END BaseModelStage.
