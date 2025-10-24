INTERFACE PicComponent;
IMPORT PicOverlay, CktElement;

TYPE
  T <: Public;

  Step = [ -1 .. +1 ];
  
  Public = PicOverlay.T OBJECT METHODS
    init(obj : CktElement.T) : T;
    setNeighbor(dx, dy : Step; n : T);
  END;

CONST Brand = "PicComponent";

END PicComponent.
