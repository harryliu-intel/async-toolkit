INTERFACE PicOverlay;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init(over, under : Pic.T) : T;
  END;

CONST Brand = "PicOverlay";

END PicOverlay.
