INTERFACE TextFormat;
IMPORT Text;

TYPE T = Text.T;
     
CONST Equal   = Text.Equal;
      Hash    = Text.Hash;
      Compare = Text.Compare;

CONST Brand = "TextFormat";

PROCEDURE Format(t : TEXT) : TEXT; (* identity *)

END TextFormat.
     
