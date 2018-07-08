GENERIC INTERFACE ModelStage(MapAddr);
IMPORT BaseModelStage;
IMPORT ServerPacket AS Pkt;
IMPORT Coroutine;
IMPORT Metadata;

TYPE
  Super = BaseModelStage.T;
  
  T = Super OBJECT
    h        : MapAddr.H;
    in, out  : Pkt.T;
    complete : BOOLEAN;
  METHODS
    (* called from outside the coroutine *)
    init (h : MapAddr.H; prev : Super) : T;
  END;

CONST Brand = "ModelStage(" & MapAddr.Brand & ")";

END ModelStage.
