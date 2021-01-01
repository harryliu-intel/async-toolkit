INTERFACE PicArray;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init() : T;
    put(x, y : INTEGER; pic : Pic.T);
  END;

CONST Brand = "PicArray";

END PicArray.
  
