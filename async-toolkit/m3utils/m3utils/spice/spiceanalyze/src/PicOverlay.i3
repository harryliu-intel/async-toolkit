INTERFACE PicOverlay;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init(over : Pic.T) : T;
  END;

CONST Brand = "PicOverlay";

END PicOverlay.
