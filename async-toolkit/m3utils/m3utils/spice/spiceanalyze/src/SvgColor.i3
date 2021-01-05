INTERFACE SvgColor;
IMPORT Word;

TYPE
  Channel = { R, G, B };

  Range   = [ 0 .. 255 ];

  T = ARRAY Channel OF Range;

CONST Brand = "SvgColor";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T) : Word.T;

CONST Black = T { 0, .. };
      White = T { LAST(Range), .. };

PROCEDURE Format(READONLY a : T) : TEXT;
  
END SvgColor.
