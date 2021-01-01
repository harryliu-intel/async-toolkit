INTERFACE PicMerge;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init() : T;
    add(pic : Pic.T);
  END;

CONST Brand = "PicMerge";

END PicMerge.
