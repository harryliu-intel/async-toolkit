INTERFACE PicComponent;
IMPORT Pic, SpiceObject;

TYPE
  T <: Public;

  Step = [ -1 .. +1 ];
  
  Public = Pic.T OBJECT METHODS
    init(obj : SpiceObject.T) : T;
    setNeighbor(dx, dy : Step; n : T);
  END;

CONST Brand = "PicComponent";

END PicComponent.
