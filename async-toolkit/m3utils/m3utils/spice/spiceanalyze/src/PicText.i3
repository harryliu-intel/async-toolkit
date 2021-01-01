INTERFACE PicText;
IMPORT Pic;

TYPE
  T <: Public;

  Public = Pic.T OBJECT METHODS
    init(text     : TEXT;
         fontSize : FontSize;
         fontType := FontType.Default;
         maxWidth : CARDINAL) : T;
  END;

  FontSize = CARDINAL;

  FontType = { Default, Serif, SansSerif };

CONST Brand = "PicText";

END PicText.
