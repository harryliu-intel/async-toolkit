MODULE MarginScenario;
IMPORT Word, Text;
FROM Fmt IMPORT F, Int;
IMPORT CheckDir;

CONST TE = Text.Equal;
      
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN
      a.datDir = b.datDir
    AND
      a.clkDir = b.clkDir
    AND
      TE(a.datNm, b.datNm)
    AND
      TE(a.clkNm, b.clkNm)
    AND
      TE(a.tag, b.tag)
  END Equal;
  

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(Text.Hash(a.clkNm), a.clkDir)
  END Hash;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN
    RETURN F("%s, %s, %s, %s, %s",
             CheckDir.Format(a.datDir),
             Int(a.clkDir),
             a.datNm,
             a.clkNm,
             a.tag)
  END Format;

BEGIN END MarginScenario.
  
