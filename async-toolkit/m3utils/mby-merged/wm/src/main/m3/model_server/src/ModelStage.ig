GENERIC INTERFACE ModelStage(TopAddr, Model);
IMPORT BaseModelStage;
IMPORT ServerPacket AS Pkt;

TYPE
  Super = BaseModelStage.T;
  
  T <: Public;

  Public = Super OBJECT
    h        : TopAddr.H;
    in, out  : Pkt.T;
    complete : BOOLEAN;
  METHODS
    (* called from outside the coroutine *)
    init (h : TopAddr.H; indices : Model.Indices; prev : Super) : T;
  END;

CONST Brand = "ModelStage(" & TopAddr.Brand & ")";

END ModelStage.
