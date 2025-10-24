MODULE GlitchGate;
IMPORT Text;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN Text.Equal(a.tgt, b.tgt)
  END Equal;
  
BEGIN
END GlitchGate.
