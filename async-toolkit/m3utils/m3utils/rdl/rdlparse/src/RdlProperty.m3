MODULE RdlProperty;
IMPORT RdlPredefProperty AS PredefI;
IMPORT Text;
IMPORT Word;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    TYPECASE a OF
      Predef(pa) =>
      TYPECASE b OF Predef(pb) => RETURN pa.x = pb.x ELSE RETURN FALSE END
    |
      Userdef(ua) =>
      TYPECASE b OF Userdef(ub) => RETURN Text.Equal(ua.nm, ub.nm) ELSE RETURN FALSE END
    ELSE
      <*ASSERT FALSE*>
    END
  END Equal;

PROCEDURE Make(nm : TEXT) : T =
  BEGIN
    FOR i := FIRST(PredefI.T) TO LAST(PredefI.T) DO
      IF Text.Equal(PredefI.Names[i],nm) THEN
        RETURN NEW(Predef, x := i)
      END
    END;

    RETURN NEW(Userdef, nm := nm);
  END Make;

PROCEDURE Format(t : T) : TEXT =
  BEGIN
    TYPECASE t OF
      Predef(p) => RETURN PredefI.Names[p.x]
    |
      Userdef(u) => RETURN u.nm
    ELSE
      <*ASSERT FALSE*>
    END
  END Format;

PROCEDURE Hash(t : T) : Word.T =
  BEGIN RETURN Text.Hash(Format(t)) END Hash;
  
BEGIN END RdlProperty.
