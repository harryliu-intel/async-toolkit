MODULE HTMLText;

REVEAL
  T = Public BRANDED "HTML Text" OBJECT
    text : TEXT;
  OVERRIDES
    init   := Init;
    format := Format;
  END;

PROCEDURE Init(self : T; text : TEXT) : T =
  BEGIN self.text := text; RETURN self END Init;

PROCEDURE Format(self : T) : TEXT = BEGIN RETURN self.text END Format;

BEGIN END HTMLText.
