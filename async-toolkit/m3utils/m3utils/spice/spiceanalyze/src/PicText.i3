INTERFACE PicText;
IMPORT Pic;
FROM Canvas IMPORT FontSize, FontType;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init(text     : TEXT;
         fontSize : FontSize;
         fontType := FontType.Default;
         maxWidth : CARDINAL) : T;
  END;

CONST Brand = "PicText";

END PicText.
