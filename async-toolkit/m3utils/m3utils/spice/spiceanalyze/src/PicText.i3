INTERFACE PicText;
IMPORT PicPoint, PicCoord;
IMPORT Word;
IMPORT PicExtent;

TYPE
  T = RECORD
    ll       : PicPoint.T;
    txt      : TEXT;
    size     : FontSize;
    width    : PicCoord.NonNeg;
    fontType : FontType;
  END;

  FontSize = PicCoord.NonNeg;

  FontType = { Default, Serif, SansSerif };

CONST Brand = "PicText";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;
  
PROCEDURE Extent(READONLY a : T) : PicExtent.T;

END PicText.
